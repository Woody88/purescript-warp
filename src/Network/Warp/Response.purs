module Network.Warp.Response where

import Prelude
import Data.Array ((:))
import Data.Either (Either(..), note)
import Data.Foldable (traverse_)
import Data.Maybe (Maybe)
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
import Network.Wai.Internal (ResponseReceived(..))
import Network.Warp.FFI.FS (createReadStreamWithRange) as FS
import Network.Warp.File (RspFileInfo(..), conditionalRequest)
import Network.Warp.FileInfo (mkFileInfo)
import Network.Warp.Header (condReqHeader, condResHeader)
import Network.Warp.Settings (Settings)
import Node.Buffer (Buffer)
import Node.Encoding (Encoding(..))
import Node.FS.Aff as FSAff
import Node.HTTP as HTTP
import Node.Stream as Stream
import Unsafe.Coerce (unsafeCoerce)

-- Response function to be used by `Application`.
sendResponse :: Settings -> Maybe Buffer -> H.RequestHeaders -> HTTP.Response -> Response -> Aff ResponseReceived
sendResponse = go
  where
  go settings buffer requestHeaders reply response = join (liftEffect (sendResponse' settings buffer requestHeaders reply response))

sendResponse' :: Settings -> Maybe Buffer -> H.RequestHeaders -> HTTP.Response -> Response -> Effect (Aff ResponseReceived)
sendResponse' settings _ _ reply (ResponseString status headers data_) = do
  let
    stream = HTTP.responseAsStream reply
  HTTP.setStatusCode reply status.code
  HTTP.setStatusMessage reply status.message
  traverse_ (setHeader (HTTP.setHeader reply)) (addServerName settings.serverName headers)
  _ <- Stream.writeString stream UTF8 data_ mempty
  Stream.end stream mempty
  pure $ pure ResponseReceived

sendResponse' settings _ _ reply (ResponseStream status headers respstream) = do
  let
    stream = HTTP.responseAsStream reply
  HTTP.setStatusCode reply status.code
  HTTP.setStatusMessage reply status.message
  traverse_ (setHeader (HTTP.setHeader reply)) (addServerName settings.serverName headers)
  _ <- Stream.pipe respstream stream
  Stream.onEnd respstream mempty
  pure $ pure ResponseReceived

-- TODO: need to find a better approach than unsafeCoerce
sendResponse' settings rawHeader _ reply (ResponseSocket cb) =
  pure do
    cb (unsafeCoerce reply) rawHeader
    pure ResponseReceived

sendResponse' settings rawH reqHead reply (ResponseFile status headers path fpart) =
  let
    sendFile404 = sendResponse settings rawH reqHead reply sendResponseFile404

    stream = HTTP.responseAsStream reply

    eFileInfo = note (error "Could not generate fileInfo") <<< mkFileInfo path
  in
    pure do
      efileStat <- try $ FSAff.stat path
      case efileStat >>= eFileInfo of
        Left e -> sendFile404
        Right fileInfo ->
          liftEffect do
            condReqH <- condReqHeader reqHead
            condResH <- condResHeader headers
            case conditionalRequest fileInfo headers condReqH condResH of
              WithoutBody s -> do
                traverse_ (setHeader (HTTP.setHeader reply)) (addServerName settings.serverName headers)
                HTTP.setStatusCode reply s.code
                Stream.end stream mempty
                pure ResponseReceived
              WithBody s h offset len -> do
                traverse_ (setHeader (HTTP.setHeader reply)) (addServerName settings.serverName h)
                HTTP.setStatusCode reply s.code
                filestream <- FS.createReadStreamWithRange path offset len
                _ <- Stream.pipe filestream stream
                Stream.onEnd filestream mempty
                pure ResponseReceived

addContentLength :: Int -> ResponseHeaders -> ResponseHeaders
addContentLength l hdrs = (hContentLength /\ show l) : hdrs

addServerName :: String -> ResponseHeaders -> ResponseHeaders
addServerName name hdrs = (hServer /\ name) : hdrs

setHeader :: (String -> String -> Effect Unit) -> Header -> Effect Unit
setHeader setF (Tuple name val) = setF (unwrap name) val

sendResponseFile404 :: Response
sendResponseFile404 = ResponseString status404 [ hContentType /\ "text/plain; charset=utf-8" ] "File not found"
