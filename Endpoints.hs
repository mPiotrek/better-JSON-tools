{-# LANGUAGE DataKinds, TypeOperators #-}

module Endpoints
( JsonToolsApi
, MinifyJob (..)
, FormatJob (..)
, FilterJob (..), FilterRes (..)
, DiffJob (..), DiffRes (..)
) where


import Data.Text (Text)
import Data.ByteString (ByteString)
import Data.Aeson (Value)
import Servant.API ((:>),(:<|>),ReqBody,Get,JSON)



type JsonToolsApi = "minify" :> ReqBody '[JSON] MinifyJob :> Get '[JSON] ByteString
               :<|> "format" :> ReqBody '[JSON] FormatJob :> Get '[JSON] ByteString
               :<|> "filter" :> ReqBody '[JSON] FilterJob :> Get '[JSON] FilterRes
               :<|> "diff"   :> ReqBody '[JSON] DiffJob   :> Get '[JSON] DiffRes
--             :<|> "all"    :> ReqBody '[JSON] _         :> Get '[JSON] _


newtype MinifyJob = MinifyJob { minifyPayload :: Value }

newtype FormatJob = FormatJob { formatPayload :: Value }

data FilterJob = FilterJob { filterPayload :: Value
                           , filterKeys :: [Text]
                           }

newtype FilterRes = FilterRes { filterResult :: Value }

data DiffJob = DiffJob { diffPayloadA :: Value
                       , diffPayloadB :: Value
                       }

data DiffRes = DiffRes { diffDeletions :: Value
                       , diffAdditions :: Value
                       }
