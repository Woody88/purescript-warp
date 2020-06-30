module Test.Warp.PathSpec where

import Prelude

import Control.Monad.Error.Class (class MonadThrow)
import Effect.Exception (Error)
import Network.Warp.Path (pathInfo')
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

pathShouldEqual :: ∀ m. MonadThrow Error m ⇒ String → Array String → m Unit
pathShouldEqual p e = pathInfo' p `shouldEqual` e

spec :: Spec Unit
spec = do
  describe "Url path" do 
    it "should parse root '/'" do 
      "/" `pathShouldEqual` []

    it "should parse /hello/world" do 
      "/hello/world" `pathShouldEqual` ["hello", "world"]

    it "should decode ignore query" do 
      "/hello/world?my=query&a=b" `pathShouldEqual` ["hello", "world"]