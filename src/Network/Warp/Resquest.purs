module Network.Warp.Request where 

import Prelude

import Data.Int as Int
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Nullable as Nullable
import Effect (Effect)
import Effect.Aff (makeAff, nonCanceler) as Aff
import Effect.Ref as Ref
import Foreign.Object as Object
import Network.HTTP.Types (Method(..), fromString) as Method
import Network.HTTP.Types.Version as Version
import Network.Wai.Internal (Request(..), RequestBodyLength(..))
import Network.Warp.FFI.HttpIncoming as HttpIncoming
import Network.Warp.FFI.Socket (fromHttpRequest) as Socket
import Node.Buffer (Buffer)
import Node.Buffer as Buffer
import Node.HTTP as HTTP
import Node.Net.Socket (remoteAddress, remotePort) as Socket
import Node.Stream as Stream
import Node.URL as Url
import Unsafe.Coerce (unsafeCoerce)

recvRequest :: HTTP.Request -> Maybe Buffer -> Effect Request  
recvRequest httpreq rawHeader = do
    remoteHost <- getRemoteHost
    pure $ 
        Request 
            { method     
            , httpVersion       
            , remoteHost
            , pathInfo 
            , queryString
            , requestHeaders
            , bodyLength
            , body
            , rawHeader: rawHeader
            , headerHost
            , headerRange: Object.lookup "range" $ HTTP.requestHeaders httpreq
            , headerUserAgent: Object.lookup "user-agent" $ HTTP.requestHeaders httpreq
            , headerReferer: Object.lookup "referer" $ HTTP.requestHeaders httpreq
            , rawPathInfo: HTTP.requestURL httpreq
            , rawQueryString: fromMaybe "" $ Nullable.toMaybe url.query
            , isSecure: false
            , nodeRequest: Just httpreq
            }


    where 
        url  = Url.parse $ HTTP.requestURL httpreq
        requestHeaders =  Object.toUnfoldable $ HTTP.requestHeaders httpreq
        pathInfo = ""

        bodyLength = do
            let (cl :: Maybe String) = Object.lookup "content-length" $ HTTP.requestHeaders httpreq
            maybe ChunkedBody (KnownLength <<< fromMaybe 0 <<< Int.fromString) cl

        headerHost = fromMaybe "" $ Object.lookup "host" $ HTTP.requestHeaders httpreq
 
        queryString = maybe Object.empty (unsafeCoerce <<< Url.parseQueryString) $ Nullable.toMaybe url.query

        method = case (Method.fromString $ HTTP.requestMethod httpreq) of 
            (Method.Custom _) -> Method.GET
            m            -> m  

        httpVersion = case HTTP.httpVersion httpreq of 
            "0.9" -> Version.http09
            "1.0" -> Version.http10
            "1.1" -> Version.http11
            "2.0" -> Version.http20
            _     -> Version.mkHttpVersion (HttpIncoming.httpVersionMajor httpreq) (HttpIncoming.httpVersionMinor httpreq)

        getRemoteHost = do 
            let socket       = Socket.fromHttpRequest httpreq
                toRemoteHost addr port = case addr, port of 
                    Just a, Just p  -> a <> ":" <> show p 
                    _, _            -> ""  

            toRemoteHost <$> Socket.remoteAddress socket <*> Socket.remotePort socket
            

        body = Aff.makeAff \done -> do
            let stream = HTTP.requestAsStream httpreq
            buffer <- Ref.new []

            Stream.onData stream \buf ->
                void $ Ref.modify (_ <> [buf]) buffer

            Stream.onEnd stream do
                bdy     <- Ref.read buffer >>= Buffer.concat 
                bodySize <- Buffer.size bdy
                done $ pure $ if bodySize == 0 then Nothing else Just bdy
            pure Aff.nonCanceler
