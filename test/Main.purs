module Test.Main where

import Prelude

import Effect (Effect)
import Effect.Aff (launchAff_)
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner (runSpec)
import Test.Warp.FileSpec as FileSpec
import Test.Warp.ServerSpec as ServerSpec

main :: Effect Unit
main = launchAff_ $ runSpec [consoleReporter] do
  FileSpec.spec
  ServerSpec.spec
