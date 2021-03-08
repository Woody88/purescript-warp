let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.14.0-20210308/packages.dhall sha256:5a86da7913f6c84adc2efacfad49ca135af8f62235e7270d9b952a8dda3c4b47

let overrides = {=}

let additions =
      { wai =
        { dependencies = [ "aff", "effect", "http-types", "node-net" ]
        , repo = "https://github.com/Woody88/purescript-wai.git"
        , version = "master"
        }
      , http-types =
        { dependencies = [ "js-uri", "tuples", "unicode" ]
        , repo = "https://github.com/Woody88/purescript-http-types.git"
        , version = "master"
        }
      }

in  upstream // overrides // additions
