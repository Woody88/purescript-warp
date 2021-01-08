module Network.Warp.Server where

import Prelude

import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Effect (Effect)
import Effect.Aff (attempt, launchAff_)
import Effect.Class (liftEffect)
import Network.Wai (Application)
import Network.Warp.FFI.Server (onRequest, onUpgrade) as FFI
import Network.Warp.Request (toWaiRequest)
import Network.Warp.Response (sendResponse)
import Network.Warp.Settings (Settings)
import Node.HTTP (Server)
import Node.HTTP as HTTP
import Node.Stream as Stream
import Unsafe.Coerce (unsafeCoerce)

onRequest :: Server -> Application -> Settings -> Effect Unit 
onRequest server app settings = FFI.onRequest server \req res -> launchAff_ do 
    waiReq <- liftEffect $ toWaiRequest req res

    let requestHeaders = _.headers $ unwrap waiReq 
        reqStream = HTTP.requestAsStream req 
        resStream = HTTP.responseAsStream res

    -- Handles request error
    liftEffect $ Stream.onError reqStream (settings.onException (Just waiReq))

    -- Handles response error
    liftEffect $ Stream.onError resStream \err -> launchAff_ do 
        sendResponse settings Nothing requestHeaders res $ settings.onExceptionResponse err

    result <- attempt $ app waiReq (sendResponse settings Nothing requestHeaders res)

    case result of 
        Left e -> sendResponse settings Nothing requestHeaders res $ settings.onExceptionResponse e
        _      -> pure unit

onUpgrade :: Server -> Application -> Settings -> Effect Unit
onUpgrade server app settings = FFI.onUpgrade server \req socket rawH -> do 
    let httpres = unsafeCoerce socket :: HTTP.Response -- Passing the socket as HTTP.Response to `sendResponse`

    waiReq <- toWaiRequest req httpres

    let requestHeaders = _.headers $ unwrap waiReq

    launchAff_ do 
        result <- attempt $ app waiReq (sendResponse settings (Just rawH) requestHeaders httpres)
        case result of 
            Left e -> sendResponse settings Nothing requestHeaders httpres $ settings.onExceptionResponse e
            _      -> pure unit