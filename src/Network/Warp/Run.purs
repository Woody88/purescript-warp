module Network.Warp.Run 
    ( runSettings
    , run
    ) 
    where

import Data.Maybe (Maybe(..))
import Effect (Effect)
import Network.Wai (Application, Request)
import Network.Warp.FFI.Server (createServer)
import Network.Warp.FFI.Server (fromHttpServer) as Server
import Network.Warp.Server (onRequest, onUpgrade) as Server
import Network.Warp.Settings (Settings, defaultSettings)
import Node.HTTP (Server)
import Node.HTTP as HTTP
import Node.Net.Server (onError) as Server
import Prelude (Unit, bind, discard, pure, void, ($))

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
    
    Server.onRequest server app settings     
    
    Server.onUpgrade server app settings 

    -- Handles response error
    Server.onError (Server.fromHttpServer server) (settings.onException (Nothing :: Maybe Request))
   
    HTTP.listen server options settings.beforeMainLoop
    
    pure server