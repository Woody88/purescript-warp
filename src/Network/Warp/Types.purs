module Network.Warp.Types  
    ( module RowApply
    , Warp 
    , WarpError
    , InvalidRequestErr 
    , InvalidRequest(..)
    ) 
    where 

import Prelude
import Data.Variant (SProxy(..), Variant, inj)
import Effect.Aff (Aff)
import Control.Monad.Except.Checked 
import Type.Row (type (+)) as RowApply
import Type.Row (type (+))

type Warp e a = ExceptV (WarpError e) Aff a

type WarpError e = (InvalidRequestErr + e)

type InvalidRequestErr e = (invalidRequest ∷ InvalidRequest | e)

data InvalidRequest = RemoteHostMissing
