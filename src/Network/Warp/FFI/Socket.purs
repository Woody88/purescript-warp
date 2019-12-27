module Network.Warp.FFI.Socket (fromHttpRequest) where 

import Node.HTTP as HTTP
import Node.Net.Socket (Socket)

foreign import fromHttpRequest :: HTTP.Request -> Socket