module Network.Warp.Settings where 

import Prelude 
import Control.Monad.Except.Checked 
import Data.Maybe (Maybe)
import Data.String.NonEmpty (unsafeFromString) as NE
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Class.Console as Console 
import Network.HTTP.Types (Port, Host(..))
import Network.HTTP.Types.Header (hContentType) as H
import Network.HTTP.Types.Status (status500) as H
import Network.Wai.Internal (Request, Response)
import Network.Wai (responseStr)
import URI.Host.RegName (unsafeFromString) as HostRegName 
import URI.Port (unsafeFromInt) as Port 
import Partial.Unsafe (unsafePartial)

type Settings 
    = { port :: Port 
      , host :: Host 
      -- , onException :: forall e. WarpException e -> Effect Unit 
      }

defaultSettings :: Settings 
defaultSettings = { port: Port.unsafeFromInt 3000
                  , host: localhost
                  -- , onException: defaultOnException
                  -- , onExceptionResponse: defaultOnExceptionResponse 
                  }
        where 
            localhost = NameAddress $ HostRegName.unsafeFromString $ unsafePartial $ NE.unsafeFromString "localhost"

-- defaultOnException :: forall e. WarpException e -> Effect Unit 
-- defaultOnException _ e = 
--   safe $ getPureScript # handleError
--     { invalidRequest: \_ -> Console.log "error"
--     }

-- defaultOnExceptionResponse :: forall someException. MonadError someException Effect => someException -> Response
-- defaultOnExceptionResponse e  = responseStr H.status500 [H.hContentType /\ "text/plain; charset=utf-8"] H.status500.message