[![License](https://img.shields.io/badge/license-MIT-blue.svg)](https://github.com/Woody88/purescript-warp/blob/master/LICENSE)
![CI](https://github.com/Woody88/purescript-warp/workflows/CI/badge.svg?branch=master)
# Warp

A server handler for WAI which wraps node's http module.

Table of Contents
-----------------

  * [Installation](#installation)
  * [Usage](#usage)
  * [Contributing](#contributing)
  * [Changelog](#changelog)
  * [License](#license)

## Installation

***This library is not yet published to pursuit.***  
You can install this package by adding the details below to your packages.dhall:

<summary><strong>Using Spago</strong></summary>

```dhall
let additions =
      { wai =
        { dependencies = [ "aff", "effect", "http-types", "node-net" ]
        , repo = "https://github.com/Woody88/purescript-wai.git"
        , version = "master"
        }
      , http-types =
        { dependencies = [ "tuples", "unicode", "generics-rep" ]
        , repo = "https://github.com/Woody88/purescript-http-types.git"
        , version = "master"
        }
      }
```

```console
user@user:~$ spago install warp
```

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
    f $ responseStr ok200 [(hContentType /\ "text/plain")] "Hello, World!"
```

## Contributing

If you are interested in fixing issues and contributing directly to the code base,
please see the [contributing guidelines](https://github.com/Woody88/purescript-warp/blob/master/CONTRIBUTING.md).

## Changelog

Change log details can be found [here](https://github.com/Woody88/purescript-warp/blob/master/CHANGELOG.md) 

## License

Licensed under the [MIT](https://github.com/Woody88/purescript-warp/blob/master/LICENSE) license.
Copyright (c) 2021 Woodson Delhia. All rights reserved.