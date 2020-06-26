module Test.Warp.FileSpec where

import Prelude

import Data.Array ((:))
import Data.Maybe (Maybe(..))
import Data.Newtype (wrap)
import Data.Tuple.Nested ((/\))
import Effect.Class (liftEffect)
import Network.HTTP.Types (ok200, partialContent206)
import Network.HTTP.Types as H
import Network.Warp.File (RspFileInfo(..), conditionalRequest)
import Network.Warp.FileInfo (mkFileInfo)
import Network.Warp.Header (condReqHeader, condResHeader)
import Node.FS.Aff as FSAff
import Node.Path (FilePath)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (fail, shouldReturn)

testFileRange :: String
              -> H.RequestHeaders 
              -> FilePath
              -> RspFileInfo
              -> Spec Unit
testFileRange desc reqhs file rsp = it desc $ do
    fstat <- FSAff.stat file 
     
    let mfileInfo = mkFileInfo file fstat
    case mfileInfo of 
      Nothing -> fail "Could not parse FileInfo"
      Just fileInfo -> case rsp of
        WithBody s hs off len -> do 
          condReqH <- liftEffect $ condReqHeader reqhs
          condResH <- liftEffect $ condResHeader hs 
          let hs' = (wrap "last-modified" /\ fileInfo.infoDate) : hs
              expected = WithBody s hs' off len
          (pure $ conditionalRequest fileInfo [] condReqH condResH) `shouldReturn` expected
        _                     -> fail "Should match WithBody Only"

spec :: Spec Unit
spec = do
  describe "conditionalRequest" $ do
      testFileRange
          "gets a file size from file system"
          [] 
          "test/fixtures/hex"
          $ WithBody ok200 [( wrap "content-length" /\ "16"),(wrap "accept-ranges" /\ "bytes") ] 0 16
      testFileRange
          "gets a file size from file system and handles Range and returns Partical Content"
          [ (wrap "range" /\ "bytes=2-14") ] 
          "test/fixtures/hex"
          $ WithBody partialContent206 [(wrap "content-range" /\ "bytes 2-14/16"), (wrap "content-length" /\ "13"), (wrap "accept-ranges"/\ "bytes")] 2 13
      testFileRange
          "truncates end point of range to file size"
          [ (wrap "range" /\ "bytes=10-20") ] 
          "test/fixtures/hex"
          $ WithBody partialContent206 [(wrap "content-range" /\ "bytes 10-15/16"), (wrap "content-length" /\ "6"), (wrap "accept-ranges" /\ "bytes")] 10 6
      testFileRange
          "gets a file size from file system and handles Range and returns OK if Range means the entire"
          [(wrap "range" /\ "bytes=0-15")] 
          "test/fixtures/hex"
          $ WithBody ok200 [(wrap "content-length" /\ "16"),(wrap "accept-ranges" /\ "bytes")] 0 16