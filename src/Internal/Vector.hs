module Internal.Vector where

import Relude

import qualified Data.Vector.Unboxed as V
import Data.Vector.Unboxed (Vector, Unbox)

splitAt :: Unbox a => Natural -> Vector a -> (Vector a, Vector a)
splitAt = V.splitAt . fromIntegral

length :: Unbox a => Vector a -> Natural
length = fromIntegral . V.length

take :: Unbox a => Natural -> Vector a -> Vector a
take = V.take . fromIntegral

drop :: Unbox a => Natural -> Vector a -> Vector a
drop = V.drop . fromIntegral

(!) :: Unbox a => Vector a -> Natural -> a
(!) v i = v V.! fromIntegral i

replicate :: Unbox a => Natural -> a -> Vector a
replicate = V.replicate . fromIntegral
