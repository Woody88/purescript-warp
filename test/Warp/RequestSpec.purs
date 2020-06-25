module Test.Warp.RequestSpec where 

-- import Prelude

-- import Data.Either (Either(..))
-- import Data.Maybe (Maybe(..))
-- import Data.Newtype (unwrap)
-- import Data.Time.Duration (Milliseconds(..))
-- import Effect (Effect)
-- import Effect.AVar (AVar)
-- import Effect.Aff (Aff, bracket, delay, forkAff, makeAff, nonCanceler)
-- import Effect.Aff.AVar as Avar
-- import Effect.Class (liftEffect)
-- import Effect.Class.Console (log)
-- import Node.HTTP (Server)
-- import Node.HTTP as HTTP
-- import Node.HTTP.Client as Client
-- import Node.Stream as Stream
-- import Test.Spec (Spec, after, after_, around, before, describe, it, pending)
-- import Test.Spec.Assertions (shouldEqual)
-- import Test.Warp.FFI.Server as FFI

-- stopServer :: Server -> Aff Unit
-- stopServer = liftEffect <<< flip HTTP.close (pure unit)

-- withStubbedApi :: ( HTTP.Request -> Aff Unit) -> Aff Unit 
-- withStubbedApi action =
--   bracket (liftEffect $ FFI.createServer)
--           stopServer
--           (\server -> do 
--               _ <- makeAff \done -> do 
--                     FFI.onRequest server \req rep -> do 
--                       done $ Right req 
--                     pure nonCanceler
--               liftEffect $ FFI.listen server 80 "localhost" $ pure unit 
--           )

-- stubRequest :: Aff { server :: HTTP.Server, bus :: AVar HTTP.Request } 
-- stubRequest = do 
--   server <- liftEffect $ FFI.createServer
--   bus <- Avar.empty
--   _ <- forkAff do 
--           httpreq <- makeAff \done -> do 
--                       FFI.onRequest server \req rep -> do 
--                         log "got request"
--                         let res = HTTP.responseAsStream rep 
--                         Stream.end res (pure unit)
--                         done $ Right req 
--                       _  <- liftEffect $ FFI.listen server 9000 "localhost" $ log "Running"
--                       pure nonCanceler
--           Avar.put httpreq bus 
          
--   pure {server, bus } 

-- simpleReq :: String -> Effect Unit
-- simpleReq uri = do
--   log ("GET " <> uri <> ":")
--   req <- Client.requestFromURI uri (const $ log "got response")
--   Stream.end (Client.requestAsStream req) (pure unit)

-- spec :: Spec Unit 
-- spec = before stubRequest $ (after \{server} -> liftEffect $ HTTP.close server (pure unit)) do
--   it "runs in NodeJS" $ \{server, bus}  -> do 
--     _  <- liftEffect $ simpleReq "http://localhost:9000"
--     req <- Avar.read bus >>= (liftEffect <<< map unwrap <<< flip recvRequest Nothing)
--     req.pathInfo `shouldEqual` []


--       -- it "runs in the browser" $ pure unit
--       -- it "supports streaming reporters" $ pure unit
--       -- it "supports async specs" do
--       --   res <- delay (Milliseconds 100.0) $> "Alligator"
--       --   res `shouldEqual` "Alligator"
--       -- it "is PureScript 0.12.x compatible" $ pure unit