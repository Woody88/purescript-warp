module Network.Warp.Run where

import Data.Either (either)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff as Aff
import Effect.Class (liftEffect)
import Effect.Exception as Ex
import Network.Wai (Application)
import Network.Warp.FFI.Server (fromHttpServer) as Server
import Network.Warp.Request (recvRequest)
import Network.Warp.Response (sendResponse)
import Network.Warp.Settings (Settings, defaultSettings)
import Node.Buffer (Buffer)
import Node.HTTP as HTTP
import Node.Net.Server (onError) as Server
import Node.Net.Socket (Socket)
import Prelude (Unit, bind, discard, pure, ($), (*>), (<<<), (>>=))
import URI.Host as Host
import Unsafe.Coerce (unsafeCoerce)

-- -- | Run an 'Application' on the given port.
-- -- | This calls 'runSettings' with 'defaultSettings'.
run :: Int -> Application -> Effect Unit
run p app = runSettings (defaultSettings { port = p }) app
    
-- TODO: need to refactor I think that there is a much better approach 
-- than this. 
runSettings :: Settings -> Application -> Effect Unit 
runSettings settings app = do 
    
    let options = { port: settings.port, hostname: Host.print settings.host, backlog: Nothing }
    server <- createServer 

    onRequest server \req res -> do 
        handleRequest settings app req Nothing Nothing res

    onUpgrade server \req socket rawHead -> do 
        handleRequest settings app req (Just socket) (Just rawHead) (unsafeCoerce socket)

    HTTP.listen server options settings.beforeMainLoop 
    Server.onError (Server.fromHttpServer server) (settings.onException Nothing)
 
handleRequest :: Settings -> Application -> HTTP.Request -> Maybe Socket -> Maybe Buffer -> HTTP.Response -> Effect Unit 
handleRequest settings app httpreq sck rawHead httpres = do 
    Aff.launchAff_ do 
        let onHandlerError  = (liftEffect <<< sendResponse settings httpres <<< settings.onExceptionResponse)
            onHandleError r e = settings.onException r e *> Ex.throwException e
        handler <- Aff.attempt $ Aff.makeAff \done -> do
                    result <- Ex.try $ recvRequest httpreq sck rawHead >>= \req -> do 
                                handle <- Ex.try $ app req (sendResponse settings httpres)
                                either (onHandleError (Just req)) pure handle
                    done $ result 
                    pure Aff.nonCanceler
        either onHandlerError pure handler 

foreign import createServer :: Effect HTTP.Server 
foreign import onRequest  :: HTTP.Server -> (HTTP.Request -> HTTP.Response -> Effect Unit) -> Effect Unit
foreign import onUpgrade :: HTTP.Server -> (HTTP.Request -> Socket -> Buffer -> Effect Unit) -> Effect Unit 