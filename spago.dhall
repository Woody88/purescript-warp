{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "my-project"
, dependencies =
    [ "checked-exceptions"
    , "console"
    , "effect"
    , "node-fs-aff"
    , "psci-support"
    , "transformers"
    , "wai"
    ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
