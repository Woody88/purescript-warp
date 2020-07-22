# Warp

A server library that wraps node's http library with an API inspired by the haskell version.   

## Installation

***This library is not yet published to pursuit.***  
You can install this package by adding it to your packages.dhall:

```dhall
let additions =
  { warp =
      { dependencies =
        [ "node-fs-aff"
        , "node-net"
        , "node-url"
        , "wai"
        ]
      , repo =
          "https://github.com/Woody88/purescript-warp.git"
      , version =
          "master"
      }
  , wai =
      { dependencies =
        [ "http-types"
        , "node-buffer"
        , "node-http"
        , "node-net"
        , "node-streams"
        , "node-url"
        ]
      , repo =
          "https://github.com/Woody88/purescript-wai.git"
      , version =
          "master"
      }
  , http-types =
      { dependencies =
          [ "console"
          , "effect"
          , "psci-support"
          , "tuples"
          , "unicode"
          , "uri"
          ]
      , repo =
          "https://github.com/Woody88/purescript-http-types.git"
      , version =
          "master"
      }
  }
```
```console
user@user:~$ spago install warp
```

## Usage 

### Hello World 
```purescript 
import Prelude

import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Class.Console as Console
import Network.HTTP.Types (ok200)
import Network.HTTP.Types.Header (hContentType)
import Network.Wai (Application, responseStr)
import Network.Warp.Run (runSettings)
import Network.Warp.Settings (defaultSettings)

main :: Effect Unit
main = do 
    let beforeMainLoop = Console.log $ "Listening on port " <> show defaultSettings.port
    void $ runSettings defaultSettings { beforeMainLoop = beforeMainLoop } app 

app :: Application 
app req f = do
    f $ responseStr ok200 [(hContentType /\ "text/plain")] "Hello World!"
```