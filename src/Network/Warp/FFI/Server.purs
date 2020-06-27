module Network.Warp.FFI.Server where 

import Prelude

import Effect (Effect)
import Effect.Aff (Milliseconds)
import Node.Buffer (Buffer)
import Node.HTTP as HTTP
import Node.Net.Server as Net
import Node.Net.Socket (Socket)

type Options = { timeout :: Milliseconds }

foreign import fromHttpServer :: HTTP.Server -> Net.Server 
foreign import createServer :: Options -> Effect HTTP.Server 
foreign import onRequest  :: HTTP.Server -> (HTTP.Request -> HTTP.Response -> Effect Unit) -> Effect Unit
foreign import onUpgrade :: HTTP.Server -> (HTTP.Request -> Socket -> Buffer -> Effect Unit) -> Effect Unit 