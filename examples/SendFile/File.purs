module Examples.SendFile.File where 


import Prelude

import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Class.Console as Console
import Network.HTTP.Types (status200)
import Network.HTTP.Types.Header (hContentType)
import Network.Wai (Application, responseFile)
import Network.Warp.Run (run)

main :: Effect Unit
main = do 
    let port = 3535
    Console.log $ "Listening on port " <> show port
    run port app 

app :: Application
app req f =
    f $ responseFile status200 [(hContentType /\ "text/html")] "myfile.html" Nothing