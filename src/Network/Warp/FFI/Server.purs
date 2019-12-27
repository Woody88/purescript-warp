module Network.Warp.FFI.Server where 

import Node.HTTP as HTTP 
import Node.Net.Server as Net 

foreign import fromHttpServer :: HTTP.Server -> Net.Server 