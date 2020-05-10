module Network.Warp.FFI.FS where

import Effect (Effect)
import Node.Path (FilePath)
import Node.Stream (Readable)

foreign import createReadStreamWithRange :: FilePath -> Int -> Int -> Effect (Readable ())