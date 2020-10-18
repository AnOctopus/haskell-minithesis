{-# LANGUAGE StrictData #-}
module Shrink where

import Relude hiding (iterate)

import Relude.Unsafe as Unsafe

import qualified Data.List.NonEmpty as NE
import qualified Data.Vector.Unboxed as V
import Data.Vector.Unboxed (Unbox, Vector, cons, snoc, (!))
import qualified System.Random as R

import Gen
import qualified Internal.Vector as VI
import Internal.Data.Tree
import Internal.Util

-- TODO: The generator seed is reset after each shrink pass, which seems incorrect and might cause bad local minima

type ShrinkTup = (Int, Int, ChoiceSeq)
type ShrinkState a = State Cache a

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

innerLoop :: (ShrinkTup -> ShrinkState ShrinkTup) -> ShrinkTup -> ShrinkState ShrinkTup
innerLoop f s = NE.last <$> shrinks
    where
        p (k, i, _l) = k > 0 || i >= 0
        shrinks = do
            triedShrinks <- iterateWhileM f s p
            let h = Unsafe.head triedShrinks -- safe because triedShrinks is guaranteed nonempty despite the type
                r = (takeWhile p) triedShrinks
            -- Include the first shrink in the list, in case none were successful
            pure $ h :| r


-- | A pass to delete blocks of choices at a time. When this works, it shrinks the choice
--   sequence significantly, which makes everything else faster.
deleteChunkPass :: Choices -> (Choices -> ShrinkState Bool) -> ShrinkState Choices
deleteChunkPass (Choices lst _idx _maxVal rSeed) f =
    -- trace ("deleted from " <> show lst <> " to " <> show final) $
    ret
    where
        inner :: ShrinkTup -> ShrinkState ShrinkTup
        inner s@(k, i, !l)
            | i >= 0 = next
            | k > 0 = pure (k `div` 2, V.length l - k - 1, l)
            | otherwise = pure s
            where
                attempt = dropWithin (fromIntegral i) (fromIntegral k) l
                    -- trace ("D " <> show attempt <> " k=" <> show k <> " i=" <> show i <> " maxVal=" <> show (V.length attempt)) $
                !next = do
                    res <- if attempt < l then f (choices attempt rSeed) else pure False
                    if | res -> pure (k, i, attempt)
                       | i > 0 && attempt ! (i-1) > 0 -> do
                             let !a2 = replace attempt (fromIntegral i-1) (attempt ! (i-1) `div` 2)
                             r2 <- f (choices a2 rSeed)
                             if r2 then pure (k, i, a2)
                                   else pure (k, i-1, l)
                       | otherwise -> pure (k, i-1, l)

        ret = do
            (_, _, final) <- innerLoop inner (8, V.length lst - 8 - 1, lst)
            pure $ choices final rSeed

zeroWithin :: (Num a, Unbox a) => Natural -> Natural -> Vector a -> Vector a
zeroWithin startIdx dropCount !l = prefix <> infix' <> suffix
    where
        prefix = VI.take startIdx l
        suffix = VI.drop (startIdx + dropCount) l
        infix' = VI.replicate dropCount 0

-- | A pass to change blocks of choices into zeroes.
zeroChunkPass :: Choices -> (Choices -> ShrinkState Bool) -> ShrinkState Choices
zeroChunkPass !(Choices lst _idx _maxVal rSeed) f =
    -- trace ("zeroed from " <> show lst <> " to " <> show final) $
    ret
    where
        inner :: ShrinkTup -> ShrinkState ShrinkTup
        inner s@(k, i, !l)
            | i >= 0 = next
            | k > 0 = pure (k `div` 2, V.length l - k, l)
            | otherwise = pure s
            where
                attempt = zeroWithin (fromIntegral i) (fromIntegral k) l
                !next = do
                    res <- if attempt < l then f (choices attempt rSeed) else pure False
                    pure if res
                        then (k, i - k, attempt)
                        else (k, i - 1, l)

        ret = do
            (_, _, final) <- innerLoop inner (8, V.length lst - 8, lst)
            pure $ choices final rSeed

shrinkChoicePass :: Choices -> (Choices -> ShrinkState Bool) -> ShrinkState Choices
shrinkChoicePass (Choices lst _idx _maxVal rSeed) f =
    -- trace ("shrunk from " <> show lst <> " to " <> show final) $
    ret
    where
        outer :: ShrinkTup -> ShrinkState ShrinkTup
        outer s@(k, i, l)
            | i >= 0 = next
            | k > 0 = pure (0, 0, l)
            | otherwise = pure s
            where
                lo = 0
                hi = l ! i
                !next = do
                    attempt <- binSearchDown f rSeed lo hi i l
                    pure (k, i-1, attempt)

        ret = do
            (_, _, final) <- innerLoop outer (1, V.length lst - 1, lst)
            pure $ choices final rSeed


binSearchDown :: (Choices -> ShrinkState Bool) -> R.StdGen -> Word64 -> Word64 -> Int -> ChoiceSeq -> ShrinkState ChoiceSeq
binSearchDown f rSeed = binSearch
    where
        binSearch :: Word64 -> Word64 -> Int -> ChoiceSeq -> ShrinkState ChoiceSeq
        binSearch lo hi i !l = if lo + 1 < hi
                                 then next
                                 else pure l
            where
                next = do
                    let mid = lo + ((hi - lo) `div` 2)
                        attempt = replace l (fromIntegral i) mid
                    res <- if attempt < l then f (choices attempt rSeed) else pure False
                    if res
                        then binSearch lo mid i attempt
                        else binSearch mid hi i l


-- | Run all the shrink passes to reach a fixed point. This ensures we finish at a local
--   minimum. Notably, a pass that made some progress is not immediately run again,
--   because it will have already picked up most of the gains it can, so it is better
--   to run other passes first, and it will always run that pass at least once more later.
shrinkToFixpoint :: Choices -> (Choices -> ShrinkState Bool) -> Choices
shrinkToFixpoint !c0 f = evalState (shrinkToFixpoint' c0 f) Internal.Data.Tree.empty


shrinkToFixpoint' :: Choices -> (Choices -> ShrinkState Bool) -> ShrinkState Choices
shrinkToFixpoint' !c0 f = c
    where
        c = do
            !c1 <- zeroChunkPass c0 f
            !c2 <- deleteChunkPass c1 f
            !c3 <- shrinkChoicePass c2 f
            if unBytes c0 == unBytes c3
                then pure c3
                else shrinkToFixpoint' c3 f
