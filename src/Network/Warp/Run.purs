module Network.Warp.Run 
    ( runSettings
    , run
    ) 
    where

import Prelude (Unit, bind, discard, pure, void, ($), (<<<))

import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (launchAff_)
import Network.Wai (Application)
import Network.Warp.FFI.Server (createServer)
import Network.Warp.FFI.Server (fromHttpServer) as Server
import Network.Warp.Http (HttpRequest)
import Network.Warp.Server (onRequest, onUpgrade) as Server 
import Network.Warp.Settings (Settings, defaultSettings)
import Node.HTTP (Server)
import Node.HTTP as HTTP
import Node.Net.Server (onError) as Server

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
    Server.onError (Server.fromHttpServer server) (launchAff_ <<< settings.onException (Nothing :: Maybe HttpRequest))
   
    HTTP.listen server options (launchAff_ settings.beforeMainLoop)
    
    pure server