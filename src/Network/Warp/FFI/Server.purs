module Network.Warp.FFI.Server where 

import Prelude

import Effect (Effect)
import Effect.Aff (Milliseconds, makeAff, nonCanceler, runAff_)
import Effect.Class (liftEffect)
import Effect.Uncurried (EffectFn3, runEffectFn3)
import Network.Wai (Middleware)
import Network.Wai.Internal (ResponseReceived(..))
import Network.Warp.Request (withNodeRequest, withNodeResponse)
import Node.Buffer (Buffer)
import Node.HTTP as HTTP
import Node.Net.Server as Net
import Node.Net.Socket (Socket)

type Options = { timeout :: Milliseconds }

-- | Adapter type for foreign middleware. i.e: express middleware
type ForeignMiddleware = EffectFn3 HTTP.Request HTTP.Response (Effect Unit) Unit

-- | Create a `Wai.Middleware` from a `ForeignMiddleware`
mkMiddlewareFromForeign :: ForeignMiddleware -> Middleware
mkMiddlewareFromForeign middleware app req send = do 
  makeAff \cb -> do 
    withNodeRequest req \nodeReq ->
      withNodeResponse req \nodeRes -> do
        runEffectFn3 middleware nodeReq nodeRes (runAff_ cb (const unit <$> app req send))
    pure nonCanceler
  pure ResponseReceived
       
foreign import fromHttpServer :: HTTP.Server -> Net.Server 
foreign import createServer :: Options -> Effect HTTP.Server 
foreign import onRequest  :: HTTP.Server -> (HTTP.Request -> HTTP.Response -> Effect Unit) -> Effect Unit
foreign import onUpgrade :: HTTP.Server -> (HTTP.Request -> Socket -> Buffer -> Effect Unit) -> Effect Unit 