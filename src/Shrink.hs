module Shrink where

import Relude
import qualified Relude.Unsafe as Unsafe

import qualified Data.List as L

import Gen

-- | Replace the value in l at index i with v
replace :: [a] -> Natural -> a -> [a]
replace l i v =
    let (h, t) = L.splitAt (fromIntegral (i - 1)) l
        r = Unsafe.tail h
    in
        case i of
            0 -> v : Unsafe.tail l
            _ -> Unsafe.head l : r <> [v] <> Unsafe.tail t

dropWithin :: Natural -> Natural -> [a] -> [a]
dropWithin startIdx dropCount l = prefix <> suffix
    where
        prefix = take (fromIntegral startIdx) l
        suffix = drop (fromIntegral $ startIdx + dropCount) l

deleteChunkPass :: Choices -> (Choices -> Bool) -> Choices
deleteChunkPass (Choices lst idx maxVal g) f = Choices final idx maxVal g
    where
        innerLoop :: Int -> Int -> [Word64] -> [Word64]
        innerLoop k i l = if i > 0
                             then next
                             else l
            where
                attempt = dropWithin (fromIntegral i) (fromIntegral k) l
                res =
                    f (Choices attempt 0 (fromIntegral $ length attempt) g)
                    -- trace (show attempt <> " k=" <> show k <> " i=" <> show i) $ f (Choices attempt 0 (fromIntegral $ length attempt) g)
                next =
                    -- trace (show res) $ if res
                      if res
                      then innerLoop k i attempt
                      else innerLoop k (i - 1) l

        outerLoop :: Int -> [Word64] -> [Word64]
        outerLoop k l = if k > 0
                          then next
                          else l
            where
                i = fromIntegral (length l - fromIntegral k - 1)
                attempt = innerLoop k i l
                next = outerLoop (k `div` 2) attempt

        final = outerLoop 8 lst

zeroWithin :: Num a => Natural -> Natural -> [a] -> [a]
zeroWithin startIdx dropCount l = prefix <> infix' <> suffix
    where
        prefix = take (fromIntegral startIdx) l
        suffix = drop (fromIntegral $ startIdx + dropCount) l
        infix' = replicate (fromIntegral dropCount) 0

zeroChunkPass :: Choices -> (Choices -> Bool) -> Choices
zeroChunkPass (Choices lst idx maxVal g) f = Choices final idx maxVal g
    where
        innerLoop :: Int -> Int -> [Word64] -> [Word64]
        innerLoop k i l = if i > 0
                          then next
                          else l
            where
                attempt = zeroWithin (fromIntegral i) (fromIntegral k) l
                res =
                    f (Choices attempt 0 (fromIntegral $ length attempt) g)
                next =
                      if res
                      then innerLoop k (i - fromIntegral k) attempt
                      else innerLoop k (i - 1) l

        outerLoop :: Int -> [Word64] -> [Word64]
        outerLoop k l = if k > 0
                        then next
                        else l
            where
                i = fromIntegral (length l - fromIntegral k - 1)
                attempt = innerLoop k i l
                next = outerLoop (k `div` 2) attempt

        final = outerLoop 8 lst

shrinkChoicePass :: Choices -> (Choices -> Bool) -> Choices
shrinkChoicePass (Choices lst _idx maxVal g) f = choices final maxVal g
    where
        binSearchLoop :: Word64 -> Word64 -> Natural -> [Word64] -> [Word64]
        binSearchLoop lo hi i l = if lo + 1 < hi
                                  then next
                                  else l
            where
                mid = lo + ((hi - lo) `div` 2)
                attempt = replace l i mid
                res = f (Choices attempt 0 maxVal g)
                next = if res
                       then binSearchLoop lo mid i l
                       else binSearchLoop mid hi i l

        outerLoop :: Natural -> [Word64] -> [Word64]
        outerLoop i l = if i > 0
                        then next
                        else l
            where
                lo = 0
                hi = l Unsafe.!! fromIntegral i
                attempt = binSearchLoop lo hi i l
                next = outerLoop (i-1) attempt

        final = outerLoop (fromIntegral (length lst - 1)) lst


shrinkToFixpoint :: Choices -> (Choices -> Bool) -> Choices
shrinkToFixpoint cs@(Choices !lst !_idx !_maxVal !_g) !f = if lst == unBytes c3
                                                    then resetIndex c3
                                                    else shrinkToFixpoint (resetIndex c3) f
    where
        c1 = deleteChunkPass cs f
        c2 = zeroChunkPass c1 f
        c3 = shrinkChoicePass c2 f
