{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "purescript-warp"
, dependencies =
  [ "avar"
  , "console"
  , "effect"
  , "node-fs-aff"
  , "node-http"
  , "node-net"
  , "node-url"
  , "generics-rep"
  , "wai"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}
