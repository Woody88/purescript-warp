module Test.Warp.FFI.Server where

import Prelude

import Effect (Effect)
import Node.HTTP as HTTP
 
foreign import createServer :: Effect HTTP.Server
foreign import onRequest :: HTTP.Server -> (HTTP.Request -> HTTP.Response -> Effect Unit) -> Effect Unit
foreign import listen :: HTTP.Server -> Int -> String -> Effect Unit -> Effect Unit