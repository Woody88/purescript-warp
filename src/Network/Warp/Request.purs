module Network.Warp.Request where

import Prelude

import Control.Monad.Maybe.Trans (MaybeT(..), runMaybeT)
import Data.Bifunctor (lmap)
import Data.Int as Int
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Newtype (wrap)
import Data.String.CaseInsensitive (CaseInsensitiveString)
import Data.Tuple (Tuple)
import Data.Vault as V
import Effect (Effect)
import Effect.Unsafe (unsafePerformEffect)
import Foreign.Object as Object
import Network.HTTP.Types (http09, http10, http11)
import Network.HTTP.Types as H
import Network.Wai (Request(..), RequestBodyLength(..))
import Node.HTTP as HTTP
import Node.Net.Socket as Socket
import Unsafe.Coerce (unsafeCoerce)

-- | Generate `Wai.Request` from `HTTP.Request`.
-- | Insert `HTTP.Request` and `HTTP.Response` in `Wai.Request` vault.
toWaiRequest :: HTTP.Request -> HTTP.Response -> Effect Request
toWaiRequest httpreq httpres = do 
  remoteHost <- getRemoteHost
  pure $ Request 
    { url
    , pathInfo
    , queryString
    , method
    , httpVersion
    , headers
    , body
    , contentLength
    , host
    , referer
    , userAgent 
    , remoteHost
    , range
    , isSecure
    , vault
    }
  where
    url         = HTTP.requestURL httpreq
    pathInfo    = H.parsePath url 
    queryString = H.parseQuery url 
    -- | Returns GET if cant parse Method
    method      = fromMaybe H.GET $ H.parseMethod $ HTTP.requestMethod httpreq  
    headers     = httpHeaders httpreq
    body        = Just $ HTTP.requestAsStream httpreq
    host        = Map.lookup (wrap "host") $ Map.fromFoldable $ httpHeaders httpreq 
    referer     = Map.lookup (wrap "referer") $ Map.fromFoldable $ httpHeaders httpreq
    range       = Map.lookup (wrap "range") $ Map.fromFoldable $ httpHeaders httpreq
    userAgent   = Map.lookup (wrap "user-agent") $ Map.fromFoldable $ httpHeaders httpreq
    isSecure    = false

    vault =
      V.empty
        # V.insert nodeRequestKey httpreq
        # V.insert nodeResponseKey httpres

    httpVersion = parseHttpVersion $ HTTP.httpVersion httpreq
        where 
            parseHttpVersion = case _ of 
                "0.9"     -> http09
                "1.1"     -> http11
                otherwise -> http10
    getRemoteHost = remoteHost' $ _.socket $ unsafeCoerce httpreq

    remoteHost' socket = runMaybeT do
      h <- MaybeT $ Socket.remoteAddress socket
      p <- MaybeT $ Socket.remotePort socket 
      pure $ h <> ":" <> show p

    contentLength = parseContentLength $ Map.lookup (wrap "content-length") $ Map.fromFoldable $ httpHeaders httpreq
      where 
          parseContentLength = 
              maybe ChunkedBody (KnownLength <<< fromMaybe 0 <<< Int.fromString) 

httpHeaders :: HTTP.Request -> Array (Tuple CaseInsensitiveString String) 
httpHeaders = map (lmap wrap) <<< Object.toUnfoldable <<< HTTP.requestHeaders

nodeRequestKey :: V.Key HTTP.Request
nodeRequestKey = unsafePerformEffect V.newKey

withNodeRequest :: forall m. Applicative m => Request -> (HTTP.Request -> m Unit) -> m Unit
withNodeRequest (Request { vault }) run = case V.lookup nodeRequestKey vault of
  Just req -> run req
  Nothing -> pure unit

nodeResponseKey :: V.Key HTTP.Response
nodeResponseKey = unsafePerformEffect V.newKey

withNodeResponse :: forall m. Applicative m => Request -> (HTTP.Response -> m Unit) -> m Unit
withNodeResponse (Request { vault }) run = case V.lookup nodeResponseKey vault of
  Just res -> run res
  Nothing -> pure unit