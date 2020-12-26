module Main where 

import Prelude 

import Effect (Effect)
import Effect.Class.Console (error)
import Effect.Class.Console as Console
import Network.HTTP.Types (ok200)
import Network.Wai (Application, responseStr)
import Network.Warp as Warp

main :: Effect Unit
main = do
  let port = 8000
  let beforeMainLoop = Console.log $ "Listening on port " <> show port
  void $ Warp.runSettings Warp.defaultSettings { beforeMainLoop = beforeMainLoop, port = port } app 

app :: Application 
app req send = send $ responseStr ok200 [] "Hello, World"