{-# LANGUAGE DataKinds, TypeOperators #-}


module Main (main) where


import Servant.Server (Server,serve)
import Servant.API ((:>),(:<|>) ((:<|>)))
import Servant (Proxy (Proxy))
import Network.Wai (Application)
import Network.Wai.Handler.Warp (run)

import Endpoints (JsonToolsApi)
import Handlers (minifyHandler,formatHandler,filterHandler,diffHandler)



type Api = "api" :> JsonToolsApi

api :: Proxy Api
api = Proxy

server :: Server Api
server = minifyHandler
    :<|> formatHandler
    :<|> filterHandler
    :<|> diffHandler

app :: Application
app = serve api server


main :: IO ()
main = run 8081 app
