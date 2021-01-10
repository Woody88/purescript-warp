module Network.Warp.Settings where 

import Effect

import Data.Maybe (Maybe)
import Data.Tuple.Nested ((/\))
import Effect.Aff (Milliseconds(..))
import Effect.Class.Console as Console
import Effect.Exception (Error)
import Effect.Exception as Error
import Network.HTTP.Types (hContentType, status500) as H
import Network.Wai (Request, Response, responseStr)
import Prelude (Unit, pure, unit, ($))

type Settings 
    = { port :: Int      
      -- ^ Port to listen on. Default value: 300
      , host :: String   
      -- ^ Default value: "127.0.0.1"
      , beforeMainLoop :: Effect Unit 
      -- ^ Will be called when server is listening^
      , onException :: Maybe Request -> Error -> Effect Unit 
      -- ^ What to do with exceptions thrown by either the application or server. Default: prints to console
      , onExceptionResponse :: Error -> Response       
      -- ^ A function to create `Response` when an exception occurs.
      -- Default: 500, text/plain, 'Internal Server Error'.
      , serverName :: String
      -- ^ Default server name: Warp/<version_number>
      , timeout :: Milliseconds
      -- ^ The number of milliseconds of inactivity before a socket is presumed to have timed out.
      -- Default 0 (no timeout)
      }

defaultSettings :: Settings 
defaultSettings = { port: 3000
                  , host: "127.0.0.1"
                  , beforeMainLoop: pure unit 
                  , onException: defaultOnException
                  , onExceptionResponse: defaultOnExceptionResponse
                  , serverName: "Warp/0.0.1"
                  , timeout: Milliseconds 0.00
                  }

defaultOnException :: Maybe Request -> Error -> Effect Unit 
defaultOnException _ e = Console.log $ Error.message e 

defaultOnExceptionResponse :: Error -> Response 
defaultOnExceptionResponse _ = 
  responseStr H.status500 [H.hContentType /\ "text/plain; charset=utf-8"] H.status500.message