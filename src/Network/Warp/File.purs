module Network.Warp.File 
  ( RspFileInfo(..)
  , Offset
  , Length
  , conditionalRequest
  ) 
  where

import Prelude

import Control.Alternative ((<|>))
import Data.Array ((:), head, null)
import Data.DateTime (DateTime, modifyTime, setMillisecond)
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Data.Maybe (Maybe(..), fromMaybe, isJust)
import Data.Tuple (Tuple)
import Data.Tuple.Nested ((/\))
import Network.HTTP.Types (ResponseHeaders)
import Network.HTTP.Types as H
import Network.Warp.FileInfo (FileInfo)
import Network.Warp.Header (RequestHeaderCond, ResponseHeaderCond)

-- | This module handles conditional get request for files
type Offset = Int 
type Length = Int 
data RspFileInfo = WithoutBody H.Status
                 | WithBody H.Status H.ResponseHeaders Offset Length   

derive instance rspFileInfoEq :: Eq RspFileInfo
derive instance rspFileInfoGeneric :: Generic RspFileInfo _
instance rspFileInfoShow :: Show RspFileInfo where 
  show = genericShow 

conditionalRequest :: 
  FileInfo
  -> ResponseHeaders
  -> RequestHeaderCond 
  -> ResponseHeaderCond
  -> RspFileInfo 
conditionalRequest fileInfo hrds reqHC resHC = case condition of
    nobody@(WithoutBody _) -> nobody
    WithBody s _ off len   -> let 
      hs1 = addContentHeaders hrds off len size
      hasLM = isJust $ resHC.lastModified
      hs = if hasLM then hs1 else lastmH <> hs1
      in WithBody s hs off len -- without -1 this will cause read stream to over read. 
  where
    lastmH = [ H.hLastModified /\ fileInfo.infoDate ]
    size  = fileInfo.size
    mcondition t =  ifmodified reqHC size t 
              <|> ifunmodified reqHC size t
              <|> ifrange reqHC size t
    condition = fromMaybe (unconditional reqHC size) (mcondition fileInfo.infoTime)

ifModifiedSince :: RequestHeaderCond -> Maybe DateTime
ifModifiedSince = _.ifModifiedSince
  
ifUnmodifiedSince :: RequestHeaderCond -> Maybe DateTime
ifUnmodifiedSince = _.ifUnmodifiedSince

ifRange :: RequestHeaderCond -> Maybe DateTime
ifRange = _.ifRange

ifmodified :: RequestHeaderCond -> Int -> DateTime -> Maybe RspFileInfo
ifmodified reqH size mtime = do
    date <- ifModifiedSince reqH
    let 
      -- TO CONSIDER: not sure if this is right but without it  
      -- we will always have a difference in milliseconds 
      d = modifyTime (setMillisecond bottom) date 
      t = modifyTime (setMillisecond bottom) mtime 
    pure $ if d /= t
           then unconditional reqH size
           else WithoutBody H.status304

ifunmodified :: RequestHeaderCond -> Int -> DateTime -> Maybe RspFileInfo
ifunmodified reqH size mtime = do
    date <- ifUnmodifiedSince reqH
    let 
      d = modifyTime (setMillisecond bottom) date 
      t = modifyTime (setMillisecond bottom) mtime
    pure $ if d == t
             then unconditional reqH size
             else WithoutBody H.preconditionFailed412

ifrange :: RequestHeaderCond -> Int -> DateTime -> Maybe RspFileInfo
ifrange reqH size mtime = do
    date <- ifRange reqH
    rng  <- reqH.range
    let 
      d = modifyTime (setMillisecond bottom) date 
      t = modifyTime (setMillisecond bottom) mtime
    pure $ if d == t
             then parseRange rng size
             else WithBody H.ok200 [] 0 size

unconditional :: RequestHeaderCond -> Int -> RspFileInfo
unconditional reqH size = case reqH.range of
  Nothing  -> WithBody H.ok200 [] 0 size
  Just rng -> parseRange rng size 

parseRange :: String -> Int -> RspFileInfo
parseRange rng size = case H.parseByteRanges rng of
  Nothing               -> WithoutBody H.requestedRangeNotSatisfiable416
  Just x 
    | null x            -> WithoutBody H.requestedRangeNotSatisfiable416
    | Just r  <- head x ->  let 
                              beg /\ end = checkRange r size
                              len = end - beg + 1
                              s = 
                                if beg == 0 && end == size - 1 
                                then H.ok200
                                else H.partialContent206
                              in WithBody s [] beg len
    | otherwise         -> WithoutBody H.status500

checkRange :: H.ByteRange -> Int -> Tuple Int Int
checkRange (H.ByteRangeFrom   beg)     size = (beg /\ (size - 1))
checkRange (H.ByteRangeFromTo beg end) size = (beg /\  min (size - 1) end)
checkRange (H.ByteRangeSuffix count)    size = (max 0 (size - count) /\ (size - 1))

-- | @contentRangeHeader beg end total@ constructs a Content-Range 'H.Header'
-- for the range specified.
contentRangeHeader :: Int -> Int -> Int -> H.Header
contentRangeHeader beg end total = (H.hContentRange /\ range)
  where
    range = 
      "bytes " 
      <> (if beg > end 
          then "*" 
          else show beg <> "-" <> show end) 
      <> ("/" <> show total)

addContentHeaders :: H.ResponseHeaders -> Int -> Int -> Int -> H.ResponseHeaders
addContentHeaders hs off len size = case unit of 
  _ | len == size -> hs'
    | otherwise   -> let 
        ctrng = contentRangeHeader off (off + len - 1) size
        in ctrng:hs'
  where
    hs' = (H.hContentLength /\ show len) : (H.hAcceptRanges /\ "bytes") : hs