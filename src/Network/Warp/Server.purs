module Network.Warp.Server where

import Prelude

import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (attempt, launchAff_)
import Effect.Class (liftEffect)
import Network.Warp.FFI.Server (onRequest, onUpgrade) as FFI
import Network.Wai (Application, headers)
import Network.Warp.Http (HttpRequest(..))
import Network.Warp.Settings (Settings)
import Network.Warp.Response (sendResponse)
import Node.HTTP (Server)
import Node.HTTP as HTTP
import Node.Stream as Stream
import Unsafe.Coerce (unsafeCoerce)

onRequest :: Server -> Application -> Settings -> Effect Unit 
onRequest server app settings = FFI.onRequest server \req res -> launchAff_ do 
    let req' = (HttpRequest req)    
        requestHeaders = headers req'
        reqStream = HTTP.requestAsStream req 
        resStream = HTTP.responseAsStream res

    -- Handles request error
    liftEffect $ Stream.onError reqStream (launchAff_ <<< settings.onException (Just req'))

    -- Handles response error
    liftEffect $ Stream.onError resStream \err -> launchAff_ do 
        sendResponse settings Nothing requestHeaders res $ settings.onExceptionResponse err

    result <- attempt $ app req' (sendResponse settings Nothing requestHeaders res)

    case result of 
        Left e -> sendResponse settings Nothing requestHeaders res $ settings.onExceptionResponse e
        _      -> pure unit

onUpgrade :: Server -> Application -> Settings -> Effect Unit
onUpgrade server app settings = FFI.onUpgrade server \req socket rawH -> do 
    let req' = (HttpRequest req)    
        requestHeaders = headers req'
        httpres = unsafeCoerce socket -- Passing the socket as HTTP.Response to `sendResponse`

    launchAff_ do 
        result <- attempt $ app req' (sendResponse settings (Just rawH) requestHeaders httpres)
        case result of 
            Left e -> sendResponse settings Nothing requestHeaders httpres $ settings.onExceptionResponse e
            _      -> pure unit