module Network.Warp.Server where

import Prelude
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Effect (Effect)
import Effect.Aff (attempt, launchAff_, runAff_)
import Network.Wai (Application)
import Network.Warp.FFI.Server (onRequest, onUpgrade) as FFI
import Network.Warp.Request (toWaiRequest)
import Network.Warp.Response (sendResponse)
import Network.Warp.Settings (Settings)
import Node.HTTP (Server)
import Node.HTTP as HTTP
import Node.Stream as Stream
import Unsafe.Coerce (unsafeCoerce)

-- | Request listener function that passes a `Wai.Request` to the `Application`.
onRequest :: Server -> Application -> Settings -> Effect Unit
onRequest server app settings =
  FFI.onRequest server \req res -> do
    waiReq <- toWaiRequest req res
    let
      requestHeaders = _.headers $ unwrap waiReq

      reqStream = HTTP.requestAsStream req

      resStream = HTTP.responseAsStream res
    -- Handles request error
    Stream.onError reqStream (settings.onException (Just waiReq))
    -- Handles response error
    Stream.onError resStream \err ->
      launchAff_ do
        sendResponse settings Nothing requestHeaders res $ settings.onExceptionResponse err
    app waiReq (sendResponse settings Nothing requestHeaders res)
      # runAff_ case _ of
          Left e -> do
            settings.onException (Just waiReq) e
            launchAff_ $ sendResponse settings Nothing requestHeaders res $ settings.onExceptionResponse e
          Right _ -> pure unit

-- | Upgrade listener function that passes `Wai.Request` and `Wai.ResponseSocket` to the `Application`
onUpgrade :: Server -> Application -> Settings -> Effect Unit
onUpgrade server app settings =
  FFI.onUpgrade server \req socket rawH -> do
    let
      httpres = unsafeCoerce socket :: HTTP.Response -- Passing the socket as HTTP.Response to `sendResponse`
    waiReq <- toWaiRequest req httpres
    let
      requestHeaders = _.headers $ unwrap waiReq
    launchAff_ do
      result <- attempt $ app waiReq (sendResponse settings (Just rawH) requestHeaders httpres)
      case result of
        Left e -> sendResponse settings Nothing requestHeaders httpres $ settings.onExceptionResponse e
        Right a -> pure a
