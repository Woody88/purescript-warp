module Examples.HelloWorld where 

import Prelude

import Effect (Effect)
import Effect.Class.Console as Console 
import Data.Tuple.Nested ((/\))
import Network.Wai (responseStr, Application)
import Network.Warp.Run (runSettings)
import Network.Warp.Settings (defaultSettings)
import Network.HTTP.Types (status200)
import Network.HTTP.Types.Header (hContentType)

main :: Effect Unit
main = do 
    let beforeMainLoop = do 
        Console.log $ "Listening on port " <> show defaultSettingds.port
    runSettings defaultSettings { beforeMainLoop = beforeMainLoop } app 

app :: Application
app req f =
    f $ responseStr status200 [(hContentType /\ "text/plain")] "Hello World!"
