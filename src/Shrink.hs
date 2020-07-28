{-# LANGUAGE StrictData #-}
module Shrink where

import Relude
import qualified Relude.Unsafe as Unsafe

import Control.DeepSeq
import qualified Data.List as L
import qualified Data.Vector.Unboxed as V
import qualified System.Random as R

import Gen


-- | Replace the value in l at index i with v
--   This seems to perform much better than the provided functions in Data.Vector.Unboxed
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

-- TODO: If suffix would be empty, we could avoid copying prefix by returning it instead of concatenating it with an empty vector
dropWithin :: V.Unbox a => Natural -> Natural -> V.Vector a -> V.Vector a
dropWithin startIdx dropCount l = prefix <> suffix
    where
        prefix = V.take (fromIntegral startIdx) l
        suffix = V.drop (fromIntegral $ startIdx + dropCount) l

deleteChunkPass :: Choices -> (Choices -> Bool) -> Choices
deleteChunkPass (Choices lst _idx _maxVal g) f =
    -- trace ("deleted from " <> show lst <> " to " <> show final) $
    choices final g
    where
        innerLoop :: Int -> Int -> V.Vector Word64 -> V.Vector Word64
        innerLoop k i !l = if i >= 0
                           then next
                           else l
            where
                attempt = dropWithin (fromIntegral i) (fromIntegral k) l
                res =
                    -- trace ("D " <> show attempt <> " k=" <> show k <> " i=" <> show i <> " maxVal=" <> show (V.length attempt)) $
                     (attempt < l) && f (choices attempt g)
                next
                    | res = innerLoop k i attempt
                    | i > 0 && attempt V.! (i-1) > 0 =
                      let !a2 = replace attempt (fromIntegral i-1) ((attempt V.! (i-1)) `div` 2) in
                          if f (choices a2 g) then innerLoop k i a2
                          else innerLoop k (i-1) l
                    | otherwise = innerLoop k (i-1) l



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
zeroChunkPass (Choices lst _idx _maxVal g) f =
    -- trace ("zeroed from " <> show lst <> " to " <> show final) $
    choices final g
    where
        innerLoop :: Int -> Int -> V.Vector Word64 -> V.Vector Word64
        innerLoop k i !l = if i >= 0
                           then next
                           else l
            where
                attempt = zeroWithin (fromIntegral i) (fromIntegral k) l
                res =
                    -- trace ("Z " <> show attempt <> " k=" <> show k <> " i=" <> show i) $
                    (attempt < l) && f (choices attempt g)
                next =
                    if res
                    then innerLoop k (i - fromIntegral k) attempt
                    else innerLoop k (i - 1) l

        outerLoop :: Int -> V.Vector Word64 -> V.Vector Word64
        outerLoop k !l = if k > 0
                         then next
                         else l
            where
                i = V.length l - k
                attempt = innerLoop k i l
                next = outerLoop (k `div` 2) attempt

        final = outerLoop 8 lst

shrinkChoicePass :: Choices -> (Choices -> Bool) -> Choices
shrinkChoicePass (Choices lst _idx _maxVal g) f =
    -- trace ("shrunk from " <> show lst <> " to " <> show final) $
    choices final g
    where
        -- init :: Word64 -> Word64 -> Int -> V.Vector Word64 -> V.Vector Word64
        -- init lo hi i l = if lo + 1 < hi
        --                  then next
        --                  else l
        --     where
        --         attempt = replace l (fromIntegral i) lo
        --         res = f (choices attempt g)
        --         next = if res
        --                then attempt
        --                else binSearchLoop lo hi i l

        outerLoop :: Int -> V.Vector Word64 -> V.Vector Word64
        outerLoop i !l = if i >= 0
                         then next
                         else l
            where
                lo = 0
                hi = l V.! i
                attempt = binSearchDown f g lo hi i l
                next = outerLoop (i-1) attempt

        final = outerLoop (V.length lst - 1) lst

binSearchDown :: (Choices -> Bool) -> R.StdGen -> Word64 -> Word64 -> Int -> V.Vector Word64 -> V.Vector Word64
binSearchDown f g = binSearch
    where
        binSearch :: Word64 -> Word64 -> Int -> V.Vector Word64 -> V.Vector Word64
        binSearch lo hi i !l = if lo + 1 < hi
                               then next
                               else l
            where
                mid = lo + ((hi - lo) `div` 2)
                attempt = replace l (fromIntegral i) mid
                res =
                    -- trace ("S " <> show attempt <> " i=" <> show i) $
                    (attempt < l) && f (choices attempt g)
                next = if res
                       then binSearch lo mid i attempt
                       else binSearch mid hi i l

-- | Run all the shrink passes to reach a fixed point. This ensures we finish at a local
--   minimum. Notably, a pass that made some progress is not immediately run again,
--   because it will have already picked up most of the gains it can, so it is better
--   to run other passes first, and it will always run that pass at least once more later.
shrinkToFixpoint :: Choices -> (Choices -> Bool) -> Choices
shrinkToFixpoint !c0 !f = if unBytes c0 == unBytes c3
                          then c3
                          else shrinkToFixpoint c3 f
    where
        c1 = zeroChunkPass c0 f
        c2 = deleteChunkPass c1 f
        c3 = shrinkChoicePass c2 f
