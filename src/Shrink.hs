{-# LANGUAGE StrictData #-}
module Shrink where

import Relude hiding (iterate)

import qualified Data.List.NonEmpty as NE
-- import qualified Data.List as L
import qualified Data.Vector.Unboxed as V
import Data.Vector.Unboxed (Unbox, Vector, cons, snoc)
import qualified System.Random as R

import Gen
import qualified Internal.Vector as VI

-- TODO: The generator seed is reset after each shrink pass, which seems incorrect and might cause bad local minima

type ShrinkState = (Int, Int, ChoiceSeq, Cache)
type ShrinkState' a = State Cache a

-- | Replace the value in l at index i with v
--   This seems to perform much better than the provided functions in Data.Vector.Unboxed
replace :: Unbox a => Vector a -> Natural -> a -> Vector a
replace l i v =
    let (h, t) = VI.splitAt i l
        r = if VI.length h == 0
            then t
            else V.tail h
    in
        case i of
            0 -> v `cons` V.tail l
            _ -> V.head l `cons` r `snoc` v <> V.tail t

-- | Return a vector with the `dropCount` elements starting at `startIdx` removed.
dropWithin :: Unbox a => Natural -> Natural -> Vector a -> Vector a
dropWithin startIdx dropCount l = prefix <> suffix
    where
        prefix = VI.take startIdx l
        suffix = VI.drop (startIdx + dropCount) l

innerLoop :: ShrinkState -> (ShrinkState -> ShrinkState) -> ShrinkState
innerLoop s f = NE.last shrinks
    where
        p (k', i', _l, _c) = k' > 0 || i' >= 0
        triedShrinks = NE.iterate f s
        -- Include the first shrink in the list, in case none were successful
        shrinks = NE.head triedShrinks :| NE.takeWhile p triedShrinks


-- | A pass to delete blocks of choices at a time. When this works, it shrinks the choice
--   sequence significantly, which makes everything else faster.
deleteChunkPass :: (Choices, Cache) -> ((Choices, Cache) -> (Bool, Cache)) -> (Choices, Cache)
deleteChunkPass ((Choices lst _idx _maxVal g), trie) f =
    -- trace ("deleted from " <> show lst <> " to " <> show final) $
    (choices final g, cache)
    where
        inner :: ShrinkState -> ShrinkState
        inner s@(k, i, !l, !c)
            | i >= 0 = next
            | k > 0 = (k `div` 2, V.length l - k - 1, l, c)
            | otherwise = s
            where
                attempt = dropWithin (fromIntegral i) (fromIntegral k) l
                (res, newCache) =
                    -- trace ("D " <> show attempt <> " k=" <> show k <> " i=" <> show i <> " maxVal=" <> show (V.length attempt)) $
                     if attempt < l then f (choices attempt g, c) else (False, c)
                next
                    | res = (k, i, attempt, newCache)
                    | i > 0 && attempt V.! (i-1) > 0 =
                      let !a2 = replace attempt (fromIntegral i-1) ((attempt V.! (i-1)) `div` 2)
                          (r2, c2) = f (choices a2 g, trie)
                      in
                          if r2 then (k, i, a2, c2)
                          else (k, i-1, l, c2)
                    | otherwise = (k, i-1, l, newCache)

        (_, _, final, cache) = innerLoop (8, V.length lst - 8 - 1, lst, trie) inner

zeroWithin :: (Num a, V.Unbox a) => Natural -> Natural -> V.Vector a -> V.Vector a
zeroWithin startIdx dropCount l = prefix <> infix' <> suffix
    where
        prefix = VI.take startIdx l
        suffix = VI.drop (startIdx + dropCount) l
        infix' = VI.replicate dropCount 0

-- | A pass to change blocks of choices into zeroes.
zeroChunkPass :: (Choices, Cache) -> ((Choices, Cache) -> (Bool, Cache)) -> (Choices, Cache)
zeroChunkPass ((Choices lst _idx _maxVal g), trie) f =
    -- trace ("zeroed from " <> show lst <> " to " <> show final) $
    (choices final g, finalCache)
    where
        inner :: ShrinkState -> ShrinkState
        inner s@(k, i, !l, c)
            | i >= 0 = next
            | k > 0 = (k `div` 2, V.length l - k, l, c)
            | otherwise = s
            where
                attempt = zeroWithin (fromIntegral i) (fromIntegral k) l
                (res, c2) =
                    -- trace ("Z " <> show attempt <> " k=" <> show k <> " i=" <> show i) $
                    if attempt < l then f (choices attempt g, trie) else (False, trie)
                next =
                    if res
                    then (k, i - k, attempt, c2)
                    else (k, i - 1, l, c2)

        (_, _, final, finalCache) = innerLoop (8, V.length lst - 8, lst, trie) inner

shrinkChoicePass :: (Choices, Cache) -> ((Choices, Cache) -> (Bool, Cache)) -> (Choices, Cache)
shrinkChoicePass ((Choices lst _idx _maxVal g), trie) f =
    -- trace ("shrunk from " <> show lst <> " to " <> show final) $
    (choices final g, finalCache)
    where
        outer :: ShrinkState -> ShrinkState
        outer s@(k, i, l, c)
            | i >= 0 = next
            | k > 0 = (0, 0, l, c)
            | otherwise = s
            where
                lo = 0
                hi = l V.! i
                (attempt, cache) = binSearchDown f g lo hi i l c
                next = (k, i-1, attempt, cache)

        (_, _, final, finalCache) = innerLoop (1, V.length lst - 1, lst, trie) outer

binSearchDown :: ((Choices, Cache) -> (Bool, Cache)) -> R.StdGen -> Word64 -> Word64 -> Int -> ChoiceSeq -> Cache -> (ChoiceSeq, Cache)
binSearchDown f g = binSearch
    where
        binSearch :: Word64 -> Word64 -> Int -> ChoiceSeq -> Cache -> (ChoiceSeq, Cache)
        binSearch lo hi i !l c = if lo + 1 < hi
                                 then next
                                 else (l, c)
            where
                mid = lo + ((hi - lo) `div` 2)
                attempt = replace l (fromIntegral i) mid
                (res, cache) =
                    -- trace ("S " <> show attempt <> " i=" <> show i) $
                    if attempt < l then f (choices attempt g, c) else (False, c)
                next = if res
                       then binSearch lo mid i attempt cache
                       else binSearch mid hi i l cache

-- | Run all the shrink passes to reach a fixed point. This ensures we finish at a local
--   minimum. Notably, a pass that made some progress is not immediately run again,
--   because it will have already picked up most of the gains it can, so it is better
--   to run other passes first, and it will always run that pass at least once more later.
shrinkToFixpoint :: (Choices, Cache) -> ((Choices, Cache) -> (Bool, Cache)) -> Choices
shrinkToFixpoint (!c0, ca0) !f = if unBytes c0 == unBytes c3
                          then c3
                          else shrinkToFixpoint (c3, ca3) f
    where
        (c1, ca1) = zeroChunkPass (c0, ca0) f
        (c2, ca2) = deleteChunkPass (c1, ca1) f
        (c3, ca3) = shrinkChoicePass (c2, ca2) f
