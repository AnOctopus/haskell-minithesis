module Shrink where

import Relude
import qualified Relude.Unsafe as Unsafe

import Data.Bits

import Gen

-- shrinkAllZero :: Bytes -> Bytes
-- shrinkAllZero bs = (0 .&.) <$> bs


dropWithin :: Natural -> Natural -> [a] -> [a]
dropWithin startIdx dropCount l = prefix <> suffix
    where
        prefix = take (fromIntegral startIdx) l
        suffix = drop (fromIntegral $ startIdx + dropCount) l

deleteChunkPass :: Choices -> (Choices -> Bool) -> Choices
deleteChunkPass (Choices lst idx maxVal g) f = Choices final idx maxVal g
    where
        innerLoop :: Natural -> Natural -> [Word64] -> [Word64]
        innerLoop k i l = if i > 0
                          then next
                          else l
            where
                attempt = dropWithin i k l
                res = trace (show attempt <> " k=" <> show k <> " i=" <> show i) $ f (Choices attempt idx (fromIntegral $ length attempt) g)
                next = trace (show res) $ if res
                      then innerLoop k i attempt
                      else innerLoop k (i - 1) l

        outerLoop :: Natural -> [Word64] -> [Word64]
        outerLoop k l = if k > 0
                        then next
                        else l
            where
                i = fromIntegral $ length l - fromIntegral k - 1
                attempt = innerLoop k i l
                next = outerLoop (fromIntegral (fromIntegral k `div` 2)) attempt

        final = outerLoop 8 lst

choiceLength :: Choices -> Natural
choiceLength (Choices w _ _ _) = fromIntegral $ length w
