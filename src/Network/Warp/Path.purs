module Network.Warp.Path where

import Prelude

import Data.Array as Array
import Data.Maybe (fromMaybe)
import Data.String (Pattern(..))
import Data.String as String
import Network.Wai (class WaiRequest)
import Network.Wai as Wai

pathInfo :: forall req. WaiRequest req => req -> Array String 
pathInfo = Wai.url >>> pathInfo'

pathInfo' :: String -> Array String 
pathInfo' = split "?" >>> first >>> split "/" >>> nonempty 
  where
    nonempty = Array.filter ((/=) "")
    split = Pattern >>> String.split
    first = Array.head >>> fromMaybe ""