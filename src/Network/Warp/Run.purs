module Network.Warp.Run where

import Data.Maybe (Maybe(..))
import Effect (Effect)
import Network.Wai (Application, Request(..))
import Network.Warp.FFI.Server (fromHttpServer) as Server
import Network.Warp.Request (recvRequest)
import Network.Warp.Response (sendResponse)
import Network.Warp.Settings (Settings, defaultSettings)
import Node.Buffer (Buffer)
import Node.HTTP (Server)
import Node.HTTP as HTTP
import Node.Net.Server (onError) as Server
import Node.Net.Socket (Socket)
import Prelude (Unit, pure, void, bind, discard, ($), (>>=))
import Unsafe.Coerce (unsafeCoerce)

-- -- | Run an 'Application' on the given port.
-- -- | This calls 'runSettings' with 'defaultSettings'.
run :: Int -> Application -> Effect Unit
run p app = void $ runSettings (defaultSettings { port = p }) app
    
-- TODO: need to refactor I think that there is a much better approach 
-- than this. 
runSettings :: Settings -> Application -> Effect Server 
runSettings settings app = do 
    
    let options = { port: settings.port, hostname: settings.host, backlog: Nothing }
    server <- createServer 

    onRequest server \req res -> do 
        handleRequest settings app req Nothing res

    onUpgrade server \req socket rawHead -> do 
        handleRequest settings app req (Just rawHead) (unsafeCoerce socket)

    HTTP.listen server options settings.beforeMainLoop 
    Server.onError (Server.fromHttpServer server) (settings.onException Nothing)

    pure server 
 
handleRequest :: Settings -> Application -> HTTP.Request -> Maybe Buffer -> HTTP.Response -> Effect Unit 
handleRequest settings app httpreq rawHead httpres = do 
    recvRequest httpreq rawHead >>= \req@(Request r) -> do 
        app req (sendResponse settings r.requestHeaders httpres)

foreign import createServer :: Effect HTTP.Server 
foreign import onRequest  :: HTTP.Server -> (HTTP.Request -> HTTP.Response -> Effect Unit) -> Effect Unit
foreign import onUpgrade :: HTTP.Server -> (HTTP.Request -> Socket -> Buffer -> Effect Unit) -> Effect Unit 