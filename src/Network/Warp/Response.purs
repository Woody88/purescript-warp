module Network.Warp.Response where 

import Prelude

import Data.Array ((:))
import Data.Either (Either(..), note)
import Data.Foldable (traverse_)
import Data.Newtype (unwrap)
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Aff (Aff, error, try)
import Effect.Class (liftEffect)
import Network.HTTP.Types.Header (Header, ResponseHeaders, hContentLength, hContentType, hServer)
import Network.HTTP.Types.Header as H
import Network.HTTP.Types.Status (status404)
import Network.Wai (Response(..))
import Network.Warp.FFI.FS (createReadStreamWithRange) as FS
import Network.Warp.File (RspFileInfo(..), conditionalRequest)
import Network.Warp.FileInfo (mkFileInfo)
import Network.Warp.Header (condReqHeader, condResHeader)
import Network.Warp.Settings (Settings)
import Node.Encoding (Encoding(..))
import Node.FS.Aff as FSAff
import Node.HTTP as HTTP
import Node.Stream as Stream
import Partial.Unsafe (unsafeCrashWith)
import Unsafe.Coerce (unsafeCoerce)

sendResponse :: Settings -> H.RequestHeaders -> HTTP.Response -> Response -> Aff Unit
sendResponse settings _ reply (ResponseString status headers data_) = do 
  let stream = HTTP.responseAsStream reply

  liftEffect do 
    _ <- traverse_ (setHeader $ HTTP.setHeader reply) $ addServerName settings.serverName headers
    _ <- HTTP.setStatusCode reply status.code 
    _ <- HTTP.setStatusMessage reply status.message

    _ <- Stream.writeString stream UTF8 data_ mempty
    
    void $ Stream.end stream mempty
 
sendResponse settings _ reply (ResponseStream status headers respstream) = do 
  let stream = HTTP.responseAsStream reply
  liftEffect do 
    _ <- traverse_ (setHeader $ HTTP.setHeader reply) $ addServerName settings.serverName headers
    _ <- HTTP.setStatusCode reply status.code 
    _ <- HTTP.setStatusMessage reply status.message
    _ <- Stream.pipe respstream stream
    Stream.onEnd respstream $ pure unit 
    
-- TODO: need to find a better approach than unsafeCoerce
sendResponse settings _ reply (ResponseSocket cb) = do 
  cb (unsafeCoerce reply)

sendResponse settings reqHead reply (ResponseFile status headers path fpart) = do
    let sendFile404 = sendResponse settings reqHead reply sendResponseFile404
        stream = HTTP.responseAsStream reply
        eFileInfo = note (error "Could not generate fileInfo") <<< mkFileInfo path

    efileStat <- try $ FSAff.stat path 
    
    case efileStat >>= eFileInfo of
        Left e        -> sendFile404 
        Right fileInfo -> do 
          condReqH <- liftEffect $ condReqHeader reqHead
          condResH <- liftEffect $ condResHeader headers 
          case conditionalRequest fileInfo headers condReqH condResH of 
            WithoutBody s -> liftEffect do 
              _ <- traverse_ (setHeader $ HTTP.setHeader reply) 
                    $ addServerName settings.serverName headers
              _ <- HTTP.setStatusCode reply s.code 
              Stream.end stream $ pure unit  
            WithBody s h offset len -> liftEffect do 
              let hdrs = addServerName settings.serverName h
              _         <- traverse_ (setHeader $ HTTP.setHeader reply) hdrs
              _         <- HTTP.setStatusCode reply s.code 
              filestream <- FS.createReadStreamWithRange path offset len 
              _         <- Stream.pipe filestream stream
              Stream.onEnd filestream $ pure unit

sendResponse settings reqHead reply (ResponseRaw bufferCallback response) =
  unsafeCrashWith "Not yet implemented"

addContentLength :: Int -> ResponseHeaders -> ResponseHeaders
addContentLength l hdrs = (hContentLength /\ show l) : hdrs

addServerName :: String -> ResponseHeaders -> ResponseHeaders 
addServerName name hdrs = (hServer /\ name) : hdrs

setHeader :: (String -> String -> Effect Unit) -> Header -> Effect Unit 
setHeader setF (Tuple name val) = setF (unwrap name) val

sendResponseFile404 :: Response
sendResponseFile404  = ResponseString status404 [hContentType /\ "text/plain; charset=utf-8"] "File not found"