module Network.Warp.Path where

import Prelude

import Data.Array as Array
import Data.Maybe (fromMaybe)
import Data.Newtype (unwrap)
import Data.String (Pattern(..))
import Data.String as String
import Network.Wai (Request)

pathInfo :: Request -> Array String 
pathInfo = unwrap >>> _.url >>> pathInfo'

pathInfo' :: String -> Array String 
pathInfo' = split "?" >>> first >>> split "/" >>> nonempty 
  where
    nonempty = Array.filter ((/=) "")
    split = Pattern >>> String.split
    first = Array.head >>> fromMaybe ""