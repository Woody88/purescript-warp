let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.13.8-20201223/packages.dhall sha256:a1a8b096175f841c4fef64c9b605fb0d691229241fd2233f6cf46e213de8a185

let overrides = {=}

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

in  upstream // overrides // additions
