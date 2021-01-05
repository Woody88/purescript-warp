module Network.Warp.FFI.Server where 

import Prelude

import Data.Either (either)
import Effect (Effect)
import Effect.Aff (Milliseconds, runAff_, throwError)
import Effect.Class (liftEffect)
import Effect.Uncurried (EffectFn3, runEffectFn3)
import Network.Wai (Middleware)
import Network.Warp.Request (withNodeRequest, withNodeResponse)
import Node.Buffer (Buffer)
import Node.HTTP as HTTP
import Node.Net.Server as Net
import Node.Net.Socket (Socket)

type Options = { timeout :: Milliseconds }

type ForeignMiddleware = EffectFn3 HTTP.Request HTTP.Response (Effect Unit) Unit

mkMiddlewareFromForeign :: ForeignMiddleware -> Middleware
mkMiddlewareFromForeign middleware app req send = liftEffect do
  withNodeRequest req \nodeReq ->
    withNodeResponse req \nodeRes ->
      runEffectFn3 middleware nodeReq nodeRes (runAff_ (either throwError pure) (app req send))
       
foreign import fromHttpServer :: HTTP.Server -> Net.Server 
foreign import createServer :: Options -> Effect HTTP.Server 
foreign import onRequest  :: HTTP.Server -> (HTTP.Request -> HTTP.Response -> Effect Unit) -> Effect Unit
foreign import onUpgrade :: HTTP.Server -> (HTTP.Request -> Socket -> Buffer -> Effect Unit) -> Effect Unit 