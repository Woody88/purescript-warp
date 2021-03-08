{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "purescript-warp"
, dependencies =
  [ "console", "effect", "node-fs-aff", "node-http", "wai" ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}
