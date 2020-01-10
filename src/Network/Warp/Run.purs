module Network.Warp.Run where

import Prelude (Unit, bind, discard, pure, ($), (*>), (<<<), (>>=))
import Data.Either (either)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Exception as Ex 
import Effect.Aff as Aff
import Network.Wai (Application)
import Network.Warp.Request (recvRequest)
import Network.Warp.Response (sendResponse)
import Network.Warp.Settings (Settings, defaultSettings)
import Network.Warp.FFI.Server (fromHttpServer) as Server 
import Node.HTTP as HTTP
import Node.Net.Server (onError) as Server 
import URI.Host as Host 

-- -- | Run an 'Application' on the given port.
-- -- | This calls 'runSettings' with 'defaultSettings'.
run :: Int -> Application -> Effect Unit
run p app = runSettings (defaultSettings { port = p }) app
    
runSettings :: Settings -> Application -> Effect Unit 
runSettings settings app = do 
    let options = { port: settings.port, hostname: Host.print settings.host, backlog: Nothing }
    server <- HTTP.createServer $ handleRequest settings app

    HTTP.listen server options settings.beforeMainLoop 
    Server.onError (Server.fromHttpServer server) (settings.onException Nothing)

handleRequest :: Settings -> Application -> HTTP.Request -> HTTP.Response -> Effect Unit 
handleRequest settings app httpreq httpres = Aff.launchAff_ do 
    let onHandlerError  = (liftEffect <<< sendResponse settings httpres <<< settings.onExceptionResponse)
        onHandleError r e = settings.onException r e *> Ex.throwException e
    handler <- Aff.attempt $ Aff.makeAff \done -> do
                result <- Ex.try $ recvRequest httpreq >>= \req -> do 
                            handle <- Ex.try $ app req (sendResponse settings httpres)
                            either (onHandleError (Just req)) pure handle
                done $ result 
                pure Aff.nonCanceler
    either onHandlerError pure handler 

