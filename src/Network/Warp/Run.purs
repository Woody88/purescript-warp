module Network.Warp.Run where

import Prelude (Unit, pure, unit, bind, flip, (>>=))
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Exception (throw)
import Network.Wai (Application)
import Network.Warp.Request (recvRequest)
import Network.Warp.Response (sendResponse)
import Network.Warp.Settings (Settings, defaultSettings)
import Node.HTTP as HTTP
import URI.Port as Port 
import URI.Host as Host 

-- -- | Run an 'Application' on the given port.
-- -- | This calls 'runSettings' with 'defaultSettings'.
run :: Int -> Application -> Effect Unit
run p app = case Port.fromInt p of 
    Just port -> runSettings (defaultSettings { port = port }) app
    Nothing   -> throw "Invalid Port Number"
    
runSettings :: Settings -> Application -> Effect Unit 
runSettings set app = do 
    let options = { port: Port.toInt set.port, hostname: Host.print set.host, backlog: Nothing }
    server <- HTTP.createServer (\req rep -> recvRequest req >>= flip app (sendResponse rep) ) 
    HTTP.listen server options (pure unit) 
