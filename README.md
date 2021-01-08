# Warp

A server library that wraps node's http library with an API inspired by the haskell version.   

## Installation

***This library is not yet published to pursuit.***  
You can install this package by adding the details below to your packages.dhall:

<details>
  <summary><strong>Using Spago</strong></summary>

```dhall
let additions =
      { wai =
        { dependencies = [ "aff", "effect", "http-types", "node-net", "vault" ]
        , repo = "https://github.com/Woody88/purescript-wai.git"
        , version = "vault"
        }
      , http-types =
        { dependencies = [ "tuples", "unicode", "generics-rep" ]
        , repo = "https://github.com/Woody88/purescript-http-types.git"
        , version = "master"
        }
      , vault =
        { dependencies =
          [ "console"
          , "effect"
          , "functions"
          , "maybe"
          , "prelude"
          , "psci-support"
          , "refs"
          ]
        , repo = "https://github.com/Woody88/purescript-vault.git"
        , version = "master"
        }
      }
```

```console
user@user:~$ spago install warp
```
</details>

</br>

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