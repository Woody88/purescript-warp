module Network.Warp.FFI.HttpIncoming where 

import Node.HTTP as HTTP

foreign import httpVersionMajor :: HTTP.Request -> Int 

foreign import httpVersionMinor :: HTTP.Request -> Int 
     