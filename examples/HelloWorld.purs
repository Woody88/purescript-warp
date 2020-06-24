module Examples.HelloWorld where 

import Prelude

import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Class.Console as Console
import Network.HTTP.Types (status200)
import Network.HTTP.Types.Header (hContentType)
import Network.Wai (responseStr)
import Network.Wai.Http (Application)
import Network.Warp.Run (runSettings)
import Network.Warp.Settings (defaultSettings)

main :: Effect Unit
main = do 
    let beforeMainLoop = do 
            Console.log $ "Listening "
        
    void $ runSettings defaultSettings { port = 0, beforeMainLoop = beforeMainLoop } app 

app :: Application 
app req f = do
    f $ responseStr status200 [(hContentType /\ "text/plain")] "Hello World!"

