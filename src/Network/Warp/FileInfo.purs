module Network.Warp.FileInfo where

import Prelude

import Data.DateTime (DateTime)
import Data.Int as Int
import Data.JSDate as JSDate
import Data.Maybe (Maybe)
import Node.FS.Stats (Stats(..))
import Node.FS.Stats as FS
import Node.Path (FilePath)

type FileInfo = 
  { name :: FilePath
  , size :: Int 
  , infoTime :: DateTime
  , infoDate :: String -- ^ Modification time in the GMT format
  }

mkFileInfo :: FilePath -> FS.Stats -> Maybe FileInfo 
mkFileInfo name (Stats fstats ) = do 
    infoTime <- JSDate.toDateTime $ fstats.mtime 
    let size  = Int.ceil fstats.size
        infoDate = JSDate.toUTCString $ JSDate.fromDateTime infoTime
    pure { name, size, infoTime, infoDate }