module Network.Warp.FFI.Server where 

import Prelude

import Data.Either (Either(..))
import Effect.Uncurried (EffectFn3, runEffectFn3)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple)
import Data.Tuple.Nested ((/\))
import Data.Vault (Key, lookup, newKey) as V
import Effect (Effect)
import Effect.Aff (Milliseconds, launchAff_, makeAff, nonCanceler, runAff_, try)
import Effect.Aff as Error
import Effect.Class (liftEffect)
import Effect.Class.Console as Console
import Network.HTTP.Types as H
import Network.Wai (Application, Middleware, Request(..), responseStr)
import Node.Buffer (Buffer)
import Node.HTTP as HTTP
import Node.Net.Server as Net
import Node.Net.Socket (Socket)

type Options = { timeout :: Milliseconds }

type HttpHandles = Tuple HTTP.Request HTTP.Response 

type ForeignApplication = EffectFn3 HTTP.Request HTTP.Response (Effect Unit) Unit

httpHandlesKey :: Effect (V.Key HttpHandles)
httpHandlesKey = V.newKey

mkMiddlewareFromForeign :: V.Key HttpHandles -> ForeignApplication -> Middleware 
mkMiddlewareFromForeign key foreignApp app req'@(Request req) send = case V.lookup key req.vault of 
  Nothing -> 
    app req' \res ->  
      send $ responseStr H.internalServerError500 [] "Http handle coult not be found with your key."
  Just (httpreq /\ httpres) ->  
    makeAff \cb -> do 
      runEffectFn3 foreignApp httpreq httpres (runAff_ cb $ app req' send) 
      pure nonCanceler
       
foreign import fromHttpServer :: HTTP.Server -> Net.Server 
foreign import createServer :: Options -> Effect HTTP.Server 
foreign import onRequest  :: HTTP.Server -> (HTTP.Request -> HTTP.Response -> Effect Unit) -> Effect Unit
foreign import onUpgrade :: HTTP.Server -> (HTTP.Request -> Socket -> Buffer -> Effect Unit) -> Effect Unit 