module Test.Main where

import Prelude

import Effect (Effect)
import Effect.Aff (launchAff_)
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner (runSpec)
import Test.Warp.FileSpec as File
import Test.Warp.PathSpec as Path
import Test.Warp.ServerSpec as Server

main :: Effect Unit
main = launchAff_ $ runSpec [consoleReporter] do
  Path.spec
  File.spec
  Server.spec
