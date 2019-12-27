module Network.Warp.Settings where 

import Prelude (Unit, unit, pure, bind, flip, ($), (>>=))
import Data.Maybe (Maybe)
import Data.Tuple.Nested ((/\))
import Data.String.NonEmpty (unsafeFromString) as NE
import Effect (Effect)
import Effect.Exception as Ex 
import Network.HTTP.Types (Port, Host(..))
import Network.HTTP.Types (hContentType, status500) as H
import Network.Wai.Internal (Request, Response)
import Network.Wai (responseStr)
import URI.Host.RegName (unsafeFromString) as HostRegName 
import URI.Port (unsafeFromInt) as Port 
import Partial.Unsafe (unsafePartial)

type Settings 
    = { port :: Int
      , host :: Host 
      , beforeMainLoop :: Effect Unit
      , onException :: Maybe Request -> Ex.Error -> Effect Unit 
      , onExceptionResponse :: Ex.Error -> Response 
      }

defaultSettings :: Settings 
defaultSettings = { port: 3000
                  , host: localhost
                  , beforeMainLoop: pure unit 
                  , onException: \_ _ -> pure unit 
                  , onExceptionResponse: defaultOnExceptionResponse
                  }
        where 
            localhost = NameAddress $ HostRegName.unsafeFromString $ unsafePartial $ NE.unsafeFromString "localhost"

defaultOnExceptionResponse :: Ex.Error -> Response 
defaultOnExceptionResponse _ = 
  responseStr H.status500 [H.hContentType /\ "text/plain; charset=utf-8"] H.status500.message