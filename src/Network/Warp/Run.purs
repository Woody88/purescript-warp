module Network.Warp.Run 
    ( runSettings
    , run
    ) 
    where

import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (attempt, launchAff_)
import Effect.Class (liftEffect)
import Network.Wai (headers)
import Network.Wai.Http (Application, HttpRequest(..))
import Network.Warp.FFI.Server (createServer, onRequest, onUpgrade)
import Network.Warp.FFI.Server (fromHttpServer) as Server
import Network.Warp.Response (sendResponse)
import Network.Warp.Settings (Settings, defaultSettings)
import Node.HTTP (Server)
import Node.HTTP as HTTP
import Node.Net.Server (onError) as Server
import Node.Stream as Stream
import Prelude (Unit, bind, discard, pure, unit, void, ($), (<<<))
import Unsafe.Coerce (unsafeCoerce)

-- -- -- | Run an 'Application' on the given port.
-- -- -- | This calls 'runSettings' with 'defaultSettings'.
run :: Int -> Application -> Effect Unit
run p app = void $ runSettings (defaultSettings { port = p }) app
    
-- -- TODO: need to refactor I think that there is a much better approach 
-- -- than this. 
runSettings ::  Settings -> Application -> Effect Server 
runSettings settings app = do 
    let options = { port: settings.port, hostname: settings.host, backlog: Nothing }

    server <- createServer { timeout: settings.timeout }
    

    onRequest server \req res -> launchAff_ do 
        let req' = (HttpRequest req)    
            requestHeaders = headers req'
            reqStream = HTTP.requestAsStream req 
            resStream = HTTP.responseAsStream res

        -- Handles request error
        liftEffect $ Stream.onError reqStream (launchAff_ <<< settings.onException (Just req'))

        -- Handles response error
        liftEffect $ Stream.onError resStream \err -> launchAff_ do 
            sendResponse settings requestHeaders res $ settings.onExceptionResponse err

        result <- attempt $ app req' (sendResponse settings requestHeaders res)

        case result of 
            Left e -> sendResponse settings requestHeaders res $ settings.onExceptionResponse e
            _      -> pure unit

    onUpgrade server \req socket _ -> do 
        let req' = (HttpRequest req)    
            requestHeaders = headers req'
            httpres = unsafeCoerce socket -- Passing the socket as HTTP.Response to `sendResponse`

        launchAff_ do 
            result <- attempt $ app req' (sendResponse settings requestHeaders httpres)
            case result of 
                Left e -> sendResponse settings requestHeaders httpres $ settings.onExceptionResponse e
                _      -> pure unit

    -- Handles response error
    Server.onError (Server.fromHttpServer server) (launchAff_ <<< settings.onException (Nothing :: Maybe HttpRequest))
   
    HTTP.listen server options (launchAff_ settings.beforeMainLoop)
    
    pure server