{-# LANGUAGE DeriveGeneric, DataKinds, TypeOperators #-}

module Endpoints
( JsonToolsApi
, MinifyJob (..)
, FormatJob (..)
, FilterJob (..), FilterRes (..)
, DiffJob (..), DiffRes (..)
) where


import Data.Text (Text)
import Data.ByteString (ByteString)
import Data.Aeson (Value,FromJSON,ToJSON)
import GHC.Generics (Generic)
import Servant.API ((:>),(:<|>),ReqBody,Get,JSON)



type JsonToolsApi = "minify" :> ReqBody '[JSON] MinifyJob :> Get '[JSON] ByteString
               :<|> "format" :> ReqBody '[JSON] FormatJob :> Get '[JSON] ByteString
               :<|> "filter" :> ReqBody '[JSON] FilterJob :> Get '[JSON] FilterRes
               :<|> "diff"   :> ReqBody '[JSON] DiffJob   :> Get '[JSON] DiffRes
--             :<|> "all"    :> ReqBody '[JSON] _         :> Get '[JSON] _


newtype MinifyJob = MinifyJob { minifyPayload :: Value } deriving (Eq,Show,Generic)

newtype FormatJob = FormatJob { formatPayload :: Value } deriving (Eq,Show,Generic)

data FilterJob = FilterJob { filterPayload :: Value
                           , filterKeys :: [Text]
                           } deriving (Eq,Show,Generic)

newtype FilterRes = FilterRes { filterResult :: Value } deriving (Eq,Show,Generic)

data DiffJob = DiffJob { diffPayloadA :: Value
                       , diffPayloadB :: Value
                       } deriving (Eq,Show,Generic)

data DiffRes = DiffRes { diffDeletions :: Value
                       , diffAdditions :: Value
                       } deriving (Eq,Show,Generic)


instance FromJSON MinifyJob
instance FromJSON FormatJob
instance FromJSON FilterJob
instance ToJSON FilterRes
instance FromJSON DiffJob
instance ToJSON DiffRes
