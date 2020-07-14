module Examples.HelloWorld where 

import Prelude

import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Class.Console as Console
import Network.HTTP.Types (ok200)
import Network.HTTP.Types.Header (hContentType)
import Network.Wai (responseStr)
import Network.Wai.Http (Application)
import Network.Warp.Run (runSettings)
import Network.Warp.Settings (defaultSettings)

main :: Effect Unit
main = do 
    let beforeMainLoop = Console.log $ "Listening on port " <> show defaultSettings.port
    void $ runSettings defaultSettings { beforeMainLoop = beforeMainLoop } app 

app :: Application 
app req f = do
    f $ responseStr ok200 [(hContentType /\ "text/plain")] "Hello World!"