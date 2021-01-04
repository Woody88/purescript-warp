module Network.Warp.Settings where 

import Effect

import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))
import Data.Vault as V
import Effect.Aff (Milliseconds(..))
import Effect.Class.Console as Console
import Effect.Exception (Error)
import Effect.Exception as Error
import Network.HTTP.Types (hContentType, status500) as H
import Network.Wai (Request, Response, responseStr)
import Network.Warp.FFI.Server (HttpHandles)
import Prelude (Unit, pure, unit, ($))

type Settings 
    = { port :: Int
      , host :: String 
      , beforeMainLoop :: Effect Unit
      , onException :: Maybe Request -> Error -> Effect Unit 
      , onExceptionResponse :: Error -> Response
      , serverName :: String
      , httpKey :: Maybe (V.Key HttpHandles)
      , timeout :: Milliseconds
      }

defaultSettings :: Settings 
defaultSettings = { port: 3000
                  , host: "127.0.0.1"
                  , beforeMainLoop: pure unit 
                  , onException: defaultOnException
                  , onExceptionResponse: defaultOnExceptionResponse
                  , serverName: "Warp/0.0.1"
                  , timeout: Milliseconds 0.00
                  , httpKey: Nothing
                  }

defaultOnException :: Maybe Request -> Error -> Effect Unit 
defaultOnException _ e = Console.log $ Error.message e 

defaultOnExceptionResponse :: Error -> Response 
defaultOnExceptionResponse _ = 
  responseStr H.status500 [H.hContentType /\ "text/plain; charset=utf-8"] H.status500.message