module Network.Warp.Settings where 

import Prelude 
import Data.String.NonEmpty (unsafeFromString) as NE
import Network.HTTP.Types (Port, Host(..))
import URI.Host.RegName (unsafeFromString) as HostRegName 
import URI.Port (unsafeFromInt) as Port 
import Partial.Unsafe (unsafePartial)

type Settings 
    = { port :: Port 
      , host :: Host 
      }

defaultSettings :: Settings 
defaultSettings = { port: Port.unsafeFromInt 3000
                  , host: localhost
                  }
        where 
            localhost = NameAddress $ HostRegName.unsafeFromString $ unsafePartial $ NE.unsafeFromString "localhost"