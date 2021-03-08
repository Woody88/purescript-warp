module Test.Warp.ServerSpec where

import Prelude

import Data.Either (Either(..), either)
import Data.Maybe (Maybe(..))
import Data.String as String
import Data.Tuple.Nested (type (/\), (/\))
import Effect.Aff (Aff, Milliseconds(..), bracket, delay, error, makeAff, nonCanceler, throwError)
import Effect.Class (liftEffect)
import Effect.Ref as Ref
import Foreign.Object as Object
import Network.HTTP.Types (hContentType, ok200, status200, status500)
import Network.Wai (Application, Middleware, Request(..), responseStr)
import Network.Warp (Settings, defaultSettings, runSettings)
import Network.Warp.FFI.Server (ForeignMiddleware, mkMiddlewareFromForeign)
import Node.Encoding (Encoding(..))
import Node.HTTP (Server)
import Node.HTTP as HTTP
import Node.HTTP.Client as Client
import Node.Net.Server as Net
import Node.Stream as Stream
import Test.Spec (Spec, around, describe, it)
import Test.Spec.Assertions (shouldEqual, shouldReturn)
import Unsafe.Coerce (unsafeCoerce)

foreign import _testMiddleware :: ForeignMiddleware

type Port = Int 
type ResponseBody = String 

getServerPort :: HTTP.Server -> Aff Net.Address
getServerPort httpserver = do 
  let netserver = unsafeCoerce httpserver
  delay $ Milliseconds 200.00  -- without this delay the os will not have the time to assign a port.
  makeAff \done -> do 
    maddr <- Net.address netserver
    case maddr of 
      Nothing   -> done <<< Left $ error "Server is not listening, verify that its properly binded to a port."
      Just addr -> done $ either Right (Left <<< error) addr
    pure nonCanceler

serveStubbedApi :: Settings -> Application -> Aff HTTP.Server
serveStubbedApi settings app = liftEffect $ runSettings settings app 

stopServer :: Server -> Aff Unit
stopServer = liftEffect <<< flip HTTP.close (pure unit)-- (Console.log "Server closed.")

testMiddleware :: Middleware 
testMiddleware = mkMiddlewareFromForeign _testMiddleware

withStubbedApi :: Settings -> Application -> (Port -> Aff Unit) -> Aff Unit
withStubbedApi settings app action =
  bracket (serveStubbedApi settings app)
          stopServer
          (\server -> action =<< _.port <$> getServerPort server) 

simpleReq :: String -> Aff (Client.Response /\ Maybe ResponseBody)
simpleReq uri = do
  bodyRef <- liftEffect $ Ref.new mempty
  res <- makeAff \done -> do 
    req <- Client.requestFromURI uri (done <<< Right)
    Stream.end (Client.requestAsStream req) (pure unit)
    pure nonCanceler

  let resStream = Client.responseAsStream res 

  liftEffect $ Stream.onDataString resStream UTF8 $ \d -> Ref.modify_ (_ <> d) bodyRef

  body <- makeAff \done -> do 
            Stream.onEnd resStream $ Ref.read bodyRef >>= (done <<< Right)
            pure nonCanceler

  pure (res /\ if String.null body then Nothing else Just body)
  
helloWorldApp :: Application
helloWorldApp (Request req) send = send $ responseStr ok200 [(hContentType /\ "text/plain")] "Hello, World!"


helloWorldThrowApp :: Application
helloWorldThrowApp req send = do 
  throwError $ error "myerror"

spec :: Spec Unit
spec = do 
  let settings = defaultSettings { port = 0 } -- Console.log "Server listening..." }
  around (withStubbedApi settings helloWorldApp) do
    describe "helloWorldApp" do
      it "should return status code 200" $ \port -> do
        response /\ _ <- simpleReq ("http://localhost:" <> show port)
        (pure $ Client.statusCode response) `shouldReturn` status200.code

      it "should return 'Hello, World!'" $ \port -> do
        _ /\ message  <- simpleReq ("http://localhost:" <> show port)
        (pure message) `shouldReturn` (Just "Hello, World!")

  around (withStubbedApi settings helloWorldThrowApp) do
    describe "helloWorldThrowApp" do
      it "should return status code 500" $ \port -> do
        response /\ _ <- simpleReq ("http://localhost:" <> show port)
        (pure $ Client.statusCode response) `shouldReturn` status500.code

      it "should return status 500 message, 'Something went wrong!'" $ \port -> do
        _ /\ message  <- simpleReq ("http://localhost:" <> show port)
        (pure message) `shouldReturn` (Just status500.message)

  around (withStubbedApi settings $ testMiddleware helloWorldApp) do
    describe "MiddlewareApp" do
      it "should find 'x-test-check' header with value 'test' in the response" $ \port -> do
        response /\ _  <- simpleReq ("http://localhost:" <> show port)

        let headers = Client.responseHeaders response 

        -- node http headers are all in lower case.
        Object.lookup "x-test-check" headers `shouldEqual` Just "test"