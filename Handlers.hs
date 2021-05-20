{-# LANGUAGE RecordWildCards, OverloadedStrings #-}

module Handlers
( minifyHandler
, formatHandler
, filterHandler
, diffHandler
) where


import Data.Foldable (foldl')
import Data.Maybe (catMaybes)
import Data.Text.Lazy (Text,toStrict)
import Data.Text.Lazy.Builder (toLazyText)
import Data.HashMap.Strict (HashMap,(!),delete,keys,fromList,union,intersection,difference)
import Data.Aeson (Value (Object))
import Data.Aeson.Text (encodeToTextBuilder)
import Data.Aeson.Encode.Pretty (encodePrettyToTextBuilder)
import Servant (Handler,throwError,err422,err500,errBody)

import Endpoints
    ( MinifyJob (..), MinifyRes (..)
    , FormatJob (..), FormatRes (..)
    , FilterJob (..), FilterRes (..)
    , DiffJob (..), DiffRes (..)
    )



minifyHandler :: MinifyJob -> Handler MinifyRes
minifyHandler MinifyJob {..} = return . MinifyRes . toLazyText . encodeToTextBuilder $ minifyPayload


formatHandler :: FormatJob -> Handler FormatRes
formatHandler FormatJob {..} = return . FormatRes . toLazyText . encodePrettyToTextBuilder $ formatPayload


filterHandler :: FilterJob -> Handler FilterRes

filterHandler
    (FilterJob (Object filterDict) filterKeys)
        = return
        . FilterRes
        . Object
        . foldl' (flip delete) filterDict
        $ toStrict <$> filterKeys

filterHandler _
    = throwError
    $ err422 { errBody = "Only objects have keys." }


jsonDiff :: Value -> Value -> Maybe (Value,Value)

jsonDiff v w | v == w = Nothing

jsonDiff (Object dictA) (Object dictB) = Just (Object diffA,Object diffB)
    where
        addedA = dictA `difference` dictB
        addedB = dictB `difference` dictA
        alteredKeys = keys $ dictA `intersection` dictB
        altered = catMaybes [withKey k $ jsonDiff (dictA ! k) (dictB ! k) | k <- alteredKeys]
            where
                withKey k (Just (x,y)) = Just ((k,x),(k,y))
                withKey k Nothing = Nothing
        alteredA = fromList $ fst <$> altered
        alteredB = fromList $ snd <$> altered
        diffA = addedA `union` alteredA
        diffB = addedB `union` alteredB

jsonDiff v w = Just (v,w)


diffHandler :: DiffJob -> Handler DiffRes
diffHandler DiffJob {..}
    = case jsonDiff diffPayloadA diffPayloadB of
        Nothing -> throwError $ err500 { errBody = "No difference not implemented." }
        Just (x,y) -> return $ DiffRes x y
