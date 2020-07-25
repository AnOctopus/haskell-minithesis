{-# LANGUAGE StrictData #-}
module Shrink where

import Relude
import qualified Relude.Unsafe as Unsafe

import Control.DeepSeq
import qualified Data.List as L
import qualified Data.Vector.Unboxed as V

import Gen


-- | Replace the value in l at index i with v
replace :: V.Unbox a => V.Vector a -> Natural -> a -> V.Vector a
replace l i v =
    let (h, t) = V.splitAt (fromIntegral i) l
        r = if V.length h == 0
            then t
            else V.tail h
    in
        case i of
            0 -> v `V.cons` V.tail l
            _ -> V.head l `V.cons` r `V.snoc` v <> V.tail t

dropWithin :: V.Unbox a => Natural -> Natural -> V.Vector a -> V.Vector a
dropWithin startIdx dropCount l = prefix <> suffix
    where
        prefix = V.take (fromIntegral startIdx) l
        suffix = V.drop (fromIntegral $ startIdx + dropCount) l

deleteChunkPass :: Choices -> (Choices -> Bool) -> Choices
deleteChunkPass (Choices lst _idx _maxVal g) f = choices final g
    where
        innerLoop :: Int -> Int -> V.Vector Word64 -> V.Vector Word64
        innerLoop k i !l = if i > 0
                           then next
                           else l
            where
                attempt = dropWithin (fromIntegral i) (fromIntegral k) l
                res =
                     not (l == attempt) && f (Choices attempt 0 (fromIntegral $ V.length attempt) g)
                    -- trace ("D " <> show attempt <> show l <> " k=" <> show k <> " i=" <> show i <> " maxVal=" <> show (V.length attempt)) $ f (choices attempt g) && not (l == attempt)
                next =
                      if res
                      then innerLoop k i attempt
                      else innerLoop k (i - 1) l

        outerLoop :: Int -> V.Vector Word64 -> V.Vector Word64
        outerLoop k !l = if k > 0
                         then next
                         else l
            where
                i = V.length l - k - 1
                attempt = innerLoop k i l
                next = outerLoop (k `div` 2) attempt

        final = outerLoop 8 lst

zeroWithin :: (Num a, V.Unbox a) => Natural -> Natural -> V.Vector a -> V.Vector a
zeroWithin startIdx dropCount l = prefix <> infix' <> suffix
    where
        prefix = V.take (fromIntegral startIdx) l
        suffix = V.drop (fromIntegral $ startIdx + dropCount) l
        infix' = V.replicate (fromIntegral dropCount) 0

zeroChunkPass :: Choices -> (Choices -> Bool) -> Choices
zeroChunkPass (Choices lst _idx _maxVal g) f = choices final g
    where
        innerLoop :: Int -> Int -> V.Vector Word64 -> V.Vector Word64
        innerLoop k i !l = if i > 0
                           then next
                           else l
            where
                attempt = zeroWithin (fromIntegral i) (fromIntegral k) l
                res =
                    -- trace ("Z " <> show attempt <> " k=" <> show k <> " i=" <> show i) $ f (choices attempt g)
                    not (l == attempt) && f (choices attempt g)
                next =
                    if res
                    then innerLoop k (i - fromIntegral k) attempt
                    else innerLoop k (i - 1) l

        outerLoop :: Int -> V.Vector Word64 -> V.Vector Word64
        outerLoop k !l = if k > 0
                         then next
                         else l
            where
                i = V.length l - k - 1
                attempt = innerLoop k i l
                next = outerLoop (k `div` 2) attempt

        final = outerLoop 8 lst

shrinkChoicePass :: Choices -> (Choices -> Bool) -> Choices
shrinkChoicePass (Choices lst _idx _maxVal g) f = choices final g
    where
        binSearchLoop :: Word64 -> Word64 -> Natural -> V.Vector Word64 -> V.Vector Word64
        binSearchLoop lo hi i !l = if lo + 1 < hi
                                   then next
                                   else l
            where
                mid = lo + ((hi - lo) `div` 2)
                attempt = replace l i mid
                res = --trace ("S " <> show attempt <> " i=" <> show i) $ f (choices attempt g)
                    not (l == attempt) && f (choices attempt g)
                next = if res
                       then binSearchLoop lo mid i l
                       else binSearchLoop mid hi i l

        outerLoop :: Natural -> V.Vector Word64 -> V.Vector Word64
        outerLoop i !l = if i > 0
                         then next
                         else l
            where
                lo = 0
                hi = l V.! fromIntegral i
                attempt = binSearchLoop lo hi i l
                next = outerLoop (i-1) attempt

        final = outerLoop (fromIntegral (V.length lst - 1)) lst


shrinkToFixpoint :: Choices -> (Choices -> Bool) -> Choices
shrinkToFixpoint !c0 !f = if unBytes c0 == unBytes c3
                          then c3
                          else shrinkToFixpoint c3 f
    where
        c1 = deleteChunkPass c0 f
        c2 = zeroChunkPass c1 f
        c3 = shrinkChoicePass c2 f
