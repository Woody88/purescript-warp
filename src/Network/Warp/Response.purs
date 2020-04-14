module Network.Warp.Response where 


import Prelude (Unit, bind, discard, pure, show, unit, ($))

import Data.Array ((:))
import Data.Either (Either(..))
import Data.Foldable (traverse_)
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Aff as Aff
import Effect.Class (liftEffect)
import Network.HTTP.Types.Header (Header, ResponseHeaders, hContentLength, hContentType, hServer)
import Network.HTTP.Types.Status (status404)
import Network.Wai.Internal (Response(..))
import Network.Warp.Settings (Settings)
import Node.Buffer (Buffer)
import Node.Buffer as Buffer
import Node.Encoding (Encoding(UTF8))
import Node.FS.Aff as FSAff
import Node.HTTP as HTTP
import Node.Stream as Stream
import Unsafe.Coerce (unsafeCoerce)

sendResponse :: Settings ->  HTTP.Response -> Response -> Effect Unit
sendResponse settings reply (ResponseString status headers body) = do 
    buffer :: Buffer <- Buffer.fromString body UTF8
    bufferSize       <- Buffer.size buffer 
    _                <- traverse_ (setHeader $ HTTP.setHeader reply) 
                            $ addContentLength bufferSize
                            $ addServerName settings.serverName headers

    _                <- HTTP.setStatusCode reply status.code 
    _                <- HTTP.setStatusMessage reply status.message

    Aff.launchAff_ $ Aff.makeAff \done -> do
        let stream = HTTP.responseAsStream reply
        _    <- Stream.write stream buffer $ pure unit
        _    <- Stream.end stream $ pure unit
        done $ Right unit
        pure Aff.nonCanceler 

sendResponse settings reply (ResponseStream status headers body) = do 
    _ <- traverse_ (setHeader $ HTTP.setHeader reply) $ addServerName settings.serverName headers
    _ <- HTTP.setStatusCode reply status.code 
    _ <- HTTP.setStatusMessage reply status.message

    Aff.launchAff_ $ Aff.makeAff \done -> do
        let stream = HTTP.responseAsStream reply
        _ <- Stream.pipe body stream
        _ <- Stream.onEnd body $ done $ Right unit
        pure Aff.nonCanceler

-- TODO: need to find a better approach than unsafeCoerce
sendResponse settings reply (ResponseSocket cb) = do 
    cb (unsafeCoerce reply)

sendResponse settings reply (ResponseFile status headers path) = 
    Aff.launchAff_ do
        let sendFile404 = liftEffect $ sendResponse settings reply sendResponseFile404
            sendFile2xx = do
                buffer <- FSAff.readFile path
                
                liftEffect $ do 
                    bufferSize <- Buffer.size buffer
                    _    <- traverse_ (setHeader $ HTTP.setHeader reply) 
                                $ addContentLength bufferSize 
                                $ addServerName settings.serverName headers

                    _    <- HTTP.setStatusCode reply status.code 
                    _    <- HTTP.setStatusMessage reply status.message

                    let stream = HTTP.responseAsStream reply
                    _ <- Stream.write stream buffer $ pure unit
                    Stream.end stream $ pure unit

        fileExist <- FSAff.exists path
        case fileExist of
            false -> sendFile404
            true  -> sendFile2xx 

addContentLength :: Int -> ResponseHeaders -> ResponseHeaders 
addContentLength l hdrs = (hContentLength /\ show l) : hdrs

addServerName :: String -> ResponseHeaders -> ResponseHeaders 
addServerName name hdrs = (hServer /\ name) : hdrs

setHeader :: (String -> String -> Effect Unit) -> Header -> Effect Unit 
setHeader setF (Tuple name val) = setF name val

sendResponseFile404 :: Response
sendResponseFile404  = ResponseString status404 [hContentType /\ "text/plain; charset=utf-8"] "File not found"