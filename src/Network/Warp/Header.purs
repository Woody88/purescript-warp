module Network.Warp.Header where

import Prelude

import Data.DateTime (DateTime)
import Data.Int as Int
import Data.JSDate as JSDate
import Data.Map as Map
import Data.Maybe (Maybe)
import Data.String as String
import Data.Traversable (traverse)
import Effect (Effect)
import Network.HTTP.Types as H

type RequestHeaderCond 
  = { ifUnmodifiedSince :: Maybe DateTime  
    , ifModifiedSince   :: Maybe DateTime 
    , ifRange          :: Maybe DateTime 
    , range            :: Maybe String
    }

type ResponseHeaderCond 
  = { contentLength :: Maybe Int  
    , lastModified :: Maybe DateTime 
    , date :: Maybe DateTime 
    }

condReqHeader :: H.RequestHeaders -> Effect RequestHeaderCond
condReqHeader reqH = do 
  {ifModifiedSince: _, ifUnmodifiedSince: _, ifRange: _, range: _} 
  <$> ifModifiedSince
  <*> ifUnmodifiedSince
  <*> ifRange
  <*> pure range
  where 
    hrd = Map.fromFoldable reqH 
    parseDatetime = map (flip bind JSDate.toDateTime)
    ifModifiedSince = parseDatetime $ traverse  JSDate.parse $ Map.lookup H.hIfModifiedSince hrd 
    ifUnmodifiedSince = parseDatetime $ traverse JSDate.parse $ Map.lookup H.hIfUnmodifiedSince hrd 
    ifRange = parseDatetime $ traverse JSDate.parse $ Map.lookup H.hIfRange hrd 
    range = Map.lookup H.hRange hrd

condResHeader :: H.RequestHeaders -> Effect ResponseHeaderCond
condResHeader reqH = do 
  {contentLength: _, lastModified: _, date: _} 
  <$> pure contentLength
  <*> lastModified
  <*> date
  where 
    hrd = Map.fromFoldable reqH 
    parseDatetime = map (flip bind JSDate.toDateTime)
    contentLength =  Int.fromString =<< Map.lookup H.hContentLength hrd 
    lastModified = parseDatetime $ traverse JSDate.parse $ Map.lookup H.hLastModified hrd 
    date = parseDatetime $ traverse JSDate.parse $ Map.lookup H.hDate hrd 
