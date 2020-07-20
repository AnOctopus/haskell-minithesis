module Shrink where

import Relude
import qualified Relude.Unsafe as Unsafe
import Data.Bits

import Gen

-- shrinkAllZero :: Bytes -> Bytes
-- shrinkAllZero bs = (0 .&.) <$> bs

-- shrinkZeroFirst :: Bytes -> Bytes
-- shrinkZeroFirst bs = 0 : Unsafe.tail bs

-- shrinkZeroFirst' :: [Bytes] -> [Bytes]
-- shrinkZeroFirst' bs = newBytes : Unsafe.tail bs
--     where
--         firstBS = Unsafe.head bs
--         newBytes = shrinkZeroFirst firstBS
