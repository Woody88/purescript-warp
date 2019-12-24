module Network.Warp.Request where 

import Prelude
import Data.Either (either, hush)
import Data.Int as Int 
import Data.Nullable as Nullable
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Effect (Effect)
import Effect.Aff (makeAff, nonCanceler) as Aff
import Effect.Aff.Class (liftAff) as Aff
import Effect.Ref as Ref
import Effect.Exception (throw)
import Foreign.Object as Object 
import Text.Parsing.Parser as P
import Network.HTTP.Types (Method(..), fromString) as Method 
import Network.HTTP.Types.Version as Version
import Network.Wai.Internal (Request(..), RequestBodyLength(..))
import Node.HTTP as HTTP 
import Node.URL as Url
import Node.Stream as Stream
import Node.Buffer as Buffer
import URI.Common (wrapParser) as Uri
import URI.Host as Host 
import URI.Path (Path(..))
import URI.Path as Path 
import URI.Query as  Query 
import URI.Extra.QueryPairs (QueryPairs(..))
import URI.Extra.QueryPairs as QueryPairs

recvRequest :: HTTP.Request -> Effect Request  
recvRequest httpreq = do
    let url         = Url.parse $ HTTP.requestURL httpreq
        queryParser = Uri.wrapParser (QueryPairs.parse pure pure) Query.parser
        requestHeaders =  Object.toUnfoldable $ HTTP.requestHeaders httpreq
        pathInfo = either (const $ Path []) identity $ P.runParser (HTTP.requestURL httpreq) Path.parser 

        bodyLength = 
            let (cl :: Maybe String) = Object.lookup "content-length" $ HTTP.requestHeaders httpreq
            in maybe ChunkedBody (KnownLength <<< fromMaybe 0 <<< Int.fromString) cl

        queryString = 
            let 
                query = P.runParser (fromMaybe "?" $ Nullable.toMaybe url.search) queryParser 
            in  either (const $ QueryPairs []) identity query
    remoteHost    <- getRemoteHost
    requestMethod <- getMethod
    httpVersion   <- getHttpVersoon

    pure $ 
        Request 
            { requestMethod     
            , httpVersion       
            , remoteHost
            , pathInfo 
            , queryString
            , requestHeaders
            , bodyLength
            , body: Aff.liftAff $ getRequestBody
            , rawPathInfo: HTTP.requestURL httpreq
            , rawQueryString: fromMaybe "" $ Nullable.toMaybe url.query
            , isSecure: false
            }

    where 
        getRemoteHost = do 
            let (maybeHostStr :: Maybe String) = Object.lookup "host" $ HTTP.requestHeaders httpreq 
                maybeHost = maybeHostStr >>= (\host -> hush $ P.runParser host Host.parser) 
            maybe (throw "remote host missing") pure maybeHost

        getMethod = case (Method.fromString $ HTTP.requestMethod httpreq) of 
            (Method.Custom _)  -> throw "method not support" -- need to change
            method   -> pure method  

        getHttpVersoon = case HTTP.httpVersion httpreq of 
            "0.9" -> pure Version.http09
            "1.0" -> pure Version.http10
            "1.1" -> pure Version.http11
            "2.0" -> pure Version.http20
            _     -> throw "http version not supported"

        getRequestBody = Aff.makeAff \done -> do
            let stream = HTTP.requestAsStream httpreq
            buffer <- Ref.new []

            Stream.onData stream \buf ->
                void $ Ref.modify (_ <> [buf]) buffer

            Stream.onEnd stream do
                body     <- Ref.read buffer >>= Buffer.concat 
                bodySize <- Buffer.size body
                done $ pure $ if bodySize == 0 then Nothing else Just body
            pure Aff.nonCanceler