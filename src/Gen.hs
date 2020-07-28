{-# LANGUAGE StrictData #-}
module Gen where

import Relude hiding ((<**>))

import qualified Data.Random.Internal.Words as W
import Data.Ratio
import qualified Data.Tree as T
import qualified Data.Vector.Unboxed as V
import qualified Relude.Unsafe as Unsafe
import qualified System.Random as R

import Internal.Util


newtype ChoiceState a = ChoiceState {runChoiceState :: StateT Choices Maybe a}
    deriving newtype (Functor, Applicative, Monad, MonadState Choices, MonadFail)
data Choices = Choices {unBytes :: V.Vector Word64, unIndex :: Index, unMaxVal :: Natural, unGen :: R.StdGen}
    deriving (Show, Generic)

instance NFData Choices

choices :: V.Vector Word64 -> R.StdGen -> Choices
choices bytes = Choices bytes 0 (fromIntegral $ V.length bytes)

newtype Gen a = Gen {runGen :: ChoiceState a}
    deriving (Functor, Applicative, Monad)


-- | Provides a value between 0 and n, by either reading from the choice vector
--   or generating a new one. It is up to the generator to interpret the result.
--   If too much data is requested, or data is requested beyond the original length
--   during shrinking, the test case is invalid and is aborted.
makeChoice :: Word64 -> ChoiceState Word64
makeChoice !n = do
    (Choices bytes !idx !maxValue !stdgen) <- get
    let
        !i = fromIntegral idx
        (!b, !stdgen') = if i < V.length bytes
            then (bytes V.! i, stdgen)
            else R.genWord64R n stdgen
        !newBytes = if i < V.length bytes
            then bytes
            else bytes `V.snoc` b
        !exitEarly = ((idx + 1) >= fromIntegral maxValue) || b > n
    if exitEarly then fail "Overrun or invalid value" else do
        put $ Choices newBytes (idx + 1) maxValue stdgen'
        pure b

-- | `makeChoice`, specialized to Int since that is the most common type to want a choice as
makeChoiceInt :: Int -> ChoiceState Int
makeChoiceInt n = do
    !v <- makeChoice $ fromIntegral n
    pure $ fromIntegral v

-- | A weighted coin flip, which results in True with probability `p`, which should be
--   between 0 and 1
weighted :: Double -> ChoiceState Bool
weighted !p
    | p >= 1 = forcedChoiceBool True
    | p <= 0 = forcedChoiceBool False
    | otherwise = do
          -- Hope 1% accuracy is good enough
          !a <- makeChoice 100
          let !i = fromIntegral a
              -- p close to 1 should be mostly true, and we want to shrink to False
              -- so i=0 should be false and the region from 1 to 1-p should be true
              !res = fromRational (i % 100) > (1-p)
          pure res

-- | Make a "choice" that is always the value given. Having a choice represented in the
--   list seems to help shrinking, even if it is a fake one.
forcedChoice :: Int -> ChoiceState Int
forcedChoice !n = do
    (Choices bytes !idx !maxValue !stdgen) <- get
    let
        !i = fromIntegral idx
        !n' = fromIntegral n
        (b, stdgen') = if i < V.length bytes
            then (bytes V.! i, stdgen)
            else (n', stdgen)
        newBytes = if i < V.length bytes
            then bytes
            else bytes `V.snoc` b
        !exitEarly = (idx + 1) > fromIntegral maxValue
    if exitEarly then fail "Overrun or invalid value" else
        put $ Choices newBytes (idx + 1) maxValue stdgen'
    pure $ fromIntegral n'

forcedChoiceBool :: Bool -> ChoiceState Bool
forcedChoiceBool !b = do
    let !b' = fromEnum b
    !c <- forcedChoice b'
    pure $ toEnum c

integral :: Integral a => Gen a
integral = Gen $ do
    !a <- makeChoice maxBound
    pure $ fromIntegral a

integralRange :: Integral a => a -> a -> Gen a
integralRange lo hi = Gen $ do
    a <- makeChoice (fromIntegral $ hi - lo)
    pure $ fromIntegral (a + fromIntegral lo)

-- | Generate an Int, up to the maximum size representable by a Word64, which is the
--   max size of an Int on x86-64 platforms. The conversion of Word64 to Int preserves
--   representation, and Ints are signed, so negative values will be produced sometimes.
--   Negative values shrink to positive ones, and values shrink to zero.
int :: Gen Int
int = integral

intRange :: Int -> Int -> Gen Int
intRange = integralRange

list :: forall a. Gen a -> Gen [a]
list !gen = Gen $ do
    !b <- weighted 0.9
    case b of
        -- stop here, return an empty list. the only stateful computation is bool choice
        False -> pure []
        -- generate an element, then append it to the results of calling list again
        True -> do
            -- It is better to draw the new value first, so that values and list-inclusion-choices
            -- are interleaved in the choice list, which makes it easier to delete the pairs
            -- and so aids shrinking
            !newVal <- runGen gen
            !l' <- runGen $ list gen
            -- The choice for the value is made before the one for the rest of the list
            -- so it should be added to the front, to match the choice order
            let newList = pure newVal <> l'
            pure newList


listRange :: Int -> Int -> Gen a -> Gen [a]
listRange !lo !hi !gen = Gen $ do
    !b <- if lo > 0
          then  weighted 1
          else if hi < 0
          then weighted 0
          else weighted 0.9
    case b of
        False -> pure []
        True -> do
            !newVal <- runGen gen
            l' <- runGen $ listRange (lo-1) (hi-1) gen
            let newList = pure newVal <> l'
            pure newList


const :: a -> Gen a
const = pure

nothing :: forall a. Gen a
nothing = Gen $ fail ""

mixOf :: [Gen a] -> Gen a
mixOf gens = Gen $ do
    idx <- makeChoiceInt $ length gens
    let g = gens Unsafe.!! idx
    runGen g

enumRange :: forall a. Enum a => a -> a -> Gen a
enumRange lo hi = Gen $ do
    let lo' = fromEnum lo
        hi' = fromEnum hi
    choice <- makeChoiceInt $ hi' - lo'
    pure . toEnum $ lo' + choice

enumBounded :: (Enum a, Bounded a) => Gen a
enumBounded = enumRange minBound maxBound

bool :: Gen Bool
bool = enumBounded

maybe :: Gen a -> Gen (Maybe a)
maybe gen = frequency [(1, pure Nothing), (1, Just <$> gen)]

frequency :: forall a. [(Int, Gen a)] -> Gen a
frequency pairs = Gen $ do
    let pick :: Int -> [(Int, Gen a)] -> Gen a
        pick n = \case
            [] -> undefined
            (k, x):xs ->
                if n <= k
                then x
                else pick (n - k) xs
        total = sum $ fst <$> pairs
    n <- makeChoiceInt total
    runGen $ pick n pairs
