module Examples.SendFile.File where 


import Prelude

import Effect (Effect)
import Effect.Class.Console as Console 
import Data.Tuple.Nested ((/\))
import Network.Wai (responseFile, Application)
import Network.Warp.Run (run)
import Network.HTTP.Types (status200)
import Network.HTTP.Types.Header (hContentType)

main :: Effect Unit
main = do 
    let port = 3000
    Console.log $ "Listening on port " <> show port
    run port app 

app :: Application
app req f =
    f $ responseFile status200 [(hContentType /\ "text/html")] "myfile.html"