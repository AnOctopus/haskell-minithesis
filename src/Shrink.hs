{-# LANGUAGE StrictData #-}
module Shrink where

import Relude
import qualified Relude.Unsafe as Unsafe

import Control.DeepSeq
import qualified Data.List as L
import qualified Data.Vector.Unboxed as V
import qualified System.Random as R

import Gen
import Internal.Data.Tree


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

-- | Return a vector with the `dropCount` elements starting at `startIdx` removed.
dropWithin :: V.Unbox a => Natural -> Natural -> V.Vector a -> V.Vector a
dropWithin startIdx dropCount l = prefix <> suffix
    where
        prefix = V.take (fromIntegral startIdx) l
        suffix = V.drop (fromIntegral $ startIdx + dropCount) l

-- | A pass to delete blocks of choices at a time. When this works, it shrinks the choice
--   sequence significantly, which makes everything else faster.
deleteChunkPass :: Choices -> (Choices -> (Bool, Cache)) -> Choices
deleteChunkPass (Choices lst _idx _maxVal g trie) f =
    -- trace ("deleted from " <> show lst <> " to " <> show final) $
    choices final g cache
    where
        innerLoop :: Int -> Int -> V.Vector Word64 -> Cache -> (V.Vector Word64, Cache)
        innerLoop k i !l !c = if i >= 0
                              then next
                              else (l, c)
            where
                attempt = dropWithin (fromIntegral i) (fromIntegral k) l
                (res, newCache) =
                    -- trace ("D " <> show attempt <> " k=" <> show k <> " i=" <> show i <> " maxVal=" <> show (V.length attempt)) $
                     if attempt < l then f (choices attempt g c) else (False, c)
                next
                    | res = innerLoop k i attempt newCache
                    | i > 0 && attempt V.! (i-1) > 0 =
                      let !a2 = replace attempt (fromIntegral i-1) ((attempt V.! (i-1)) `div` 2)
                          (r2, c2) = f (choices a2 g trie)
                      in
                          if r2 then innerLoop k i a2 c2
                          else innerLoop k (i-1) l c2
                    | otherwise = innerLoop k (i-1) l newCache

        outerLoop :: Int -> V.Vector Word64 -> Cache -> (V.Vector Word64, Cache)
        outerLoop k !l !c = if k > 0
                            then next
                            else (l, c)
            where
                i = V.length l - k - 1
                (attempt, iCache) = innerLoop k i l c
                next = outerLoop (k `div` 2) attempt iCache

        (final, cache) = outerLoop 8 lst trie

zeroWithin :: (Num a, V.Unbox a) => Natural -> Natural -> V.Vector a -> V.Vector a
zeroWithin startIdx dropCount l = prefix <> infix' <> suffix
    where
        prefix = V.take (fromIntegral startIdx) l
        suffix = V.drop (fromIntegral $ startIdx + dropCount) l
        infix' = V.replicate (fromIntegral dropCount) 0

zeroChunkPass :: Choices -> (Choices -> (Bool, Cache)) -> Choices
zeroChunkPass (Choices lst _idx _maxVal g trie) f =
    -- trace ("zeroed from " <> show lst <> " to " <> show final) $
    choices final g finalCache
    where
        innerLoop :: Int -> Int -> V.Vector Word64 -> Cache -> (V.Vector Word64, Cache)
        innerLoop k i !l c = if i >= 0
                             then next
                             else (l, c)
            where
                attempt = zeroWithin (fromIntegral i) (fromIntegral k) l
                (res, c2) =
                    -- trace ("Z " <> show attempt <> " k=" <> show k <> " i=" <> show i) $
                    if attempt < l then f (choices attempt g trie) else (False, trie)
                next =
                    if res
                    then innerLoop k (i - fromIntegral k) attempt c2
                    else innerLoop k (i - 1) l c2

        outerLoop :: Int -> V.Vector Word64 -> Cache -> (V.Vector Word64, Cache)
        outerLoop k !l c = if k > 0
                           then next
                           else (l, c)
            where
                i = V.length l - k
                (attempt, cache) = innerLoop k i l c
                next = outerLoop (k `div` 2) attempt cache

        (final, finalCache) = outerLoop 8 lst trie

shrinkChoicePass :: Choices -> (Choices -> (Bool, Cache)) -> Choices
shrinkChoicePass (Choices lst _idx _maxVal g trie) f =
    -- trace ("shrunk from " <> show lst <> " to " <> show final) $
    choices final g finalCache
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

        outerLoop :: Int -> V.Vector Word64 -> Cache -> (V.Vector Word64, Cache)
        outerLoop i !l c = if i >= 0
                         then next
                         else (l, c)
            where
                lo = 0
                hi = l V.! i
                (attempt, cache) = binSearchDown f g lo hi i l c
                next = outerLoop (i-1) attempt cache

        (final, finalCache) = outerLoop (V.length lst - 1) lst trie

binSearchDown :: (Choices -> (Bool, Cache)) -> R.StdGen -> Word64 -> Word64 -> Int -> V.Vector Word64 -> Cache -> (V.Vector Word64, Cache)
binSearchDown f g = binSearch
    where
        binSearch :: Word64 -> Word64 -> Int -> V.Vector Word64 -> Cache -> (V.Vector Word64, Cache)
        binSearch lo hi i !l c = if lo + 1 < hi
                                 then next
                                 else (l, c)
            where
                mid = lo + ((hi - lo) `div` 2)
                attempt = replace l (fromIntegral i) mid
                (res, cache) =
                    -- trace ("S " <> show attempt <> " i=" <> show i) $
                    if attempt < l then f (choices attempt g c) else (False, c)
                next = if res
                       then binSearch lo mid i attempt cache
                       else binSearch mid hi i l cache

-- | Run all the shrink passes to reach a fixed point. This ensures we finish at a local
--   minimum. Notably, a pass that made some progress is not immediately run again,
--   because it will have already picked up most of the gains it can, so it is better
--   to run other passes first, and it will always run that pass at least once more later.
shrinkToFixpoint :: Choices -> (Choices -> (Bool, Cache)) -> Choices
shrinkToFixpoint !c0 !f = if unBytes c0 == unBytes c3
                          then c3
                          else shrinkToFixpoint c3 f
    where
        c1 = zeroChunkPass c0 f
        c2 = deleteChunkPass c1 f
        c3 = shrinkChoicePass c2 f
