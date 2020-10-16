{-# LANGUAGE StrictData #-}
module Gen where

import Relude
import qualified Relude.Unsafe as Unsafe

import Data.Bits
import qualified Data.Vector.Unboxed as V
import qualified System.Random as R

import qualified GHC.Float as F

import Internal.Util
import Internal.Data.Tree


data TestResult = Overrun
                | Boring
                | Interesting
    deriving (Show, Eq, Generic)

interesting :: TestResult -> Bool
interesting Interesting = True
interesting _ = False

data PropertyResult a = Valid
                      | Invalid
                      | Failure a a
                      | AssertFailure String
    deriving (Show, Eq, Generic)

asTestResult :: PropertyResult a -> TestResult
asTestResult = \case
    Valid -> Boring
    Invalid -> Overrun
    Failure _ _ -> Interesting
    AssertFailure _ -> Interesting

interestingProp :: PropertyResult a -> Bool
interestingProp (Failure _ _) = True
interestingProp (AssertFailure _) = True
interestingProp _ = False

type ChoiceSeq = V.Vector Word64

newtype ChoiceState a = ChoiceState {runChoiceState :: StateT Choices Maybe a}
    deriving newtype (Functor, Applicative, Monad, MonadState Choices, MonadFail)
data Choices = Choices {
    unBytes :: ChoiceSeq,
    unIndex :: Index,
    unMaxVal :: Natural,
    unGen :: R.StdGen
    }
    deriving (Show, Generic)

type Cache = MapTrie TestResult

instance NFData TestResult
instance NFData Cache
instance NFData Choices

choices :: ChoiceSeq -> R.StdGen -> Choices
choices bytes = Choices bytes 0 (fromIntegral $ V.length bytes)

newtype Gen a = Gen {runGen :: ChoiceState a}
    deriving (Functor, Applicative, Monad)


-- | Provides a value between 0 and n, by either reading from the choice vector
--   or generating a new one. It is up to the generator to interpret the result.
--   If too much data is requested, or data is requested beyond the original length
--   during shrinking, the test case is invalid and is aborted.
makeChoice :: Word64 -> ChoiceState Word64
makeChoice !n = makeChoiceFn n (`mod` n)

-- | Produces a choice between 0 and n, where if the choice is generated, it is interpreted
--   from a Word64 by a provided function. This allows for customizing the generation of
--   choices directly, rather than doing so by having a generator interpret a choice value
--   in some specific way. A max value for the choice must still be provided even though
--   it is not used in generation, because shrinking can cause choice values generated in
--   other ways to be used, so we still need to be able to reject ones that are too big.
makeChoiceFn :: Word64 -> (Word64 -> Word64) -> ChoiceState Word64
makeChoiceFn !n !f = do
    (Choices bytes !idx !maxValue !stdgen) <- get
    let
        !i = fromIntegral idx
        (!b, !stdgen') = if i < V.length bytes
            then (bytes V.! i, stdgen)
            else let (!b', !g') = R.genWord64 stdgen in (f b', g')
        !newBytes = if i < V.length bytes
            then bytes
            else bytes `V.snoc` b
        !exitEarly = ((idx + 1) > fromIntegral maxValue) || b > n
    if exitEarly then -- trace (show idx <> " " <> show maxValue <> " " <> show b <> " " <> show n) $
        fail "Overrun or invalid value" else do
        put $ Choices newBytes (idx + 1) maxValue stdgen'
        pure b

-- | `makeChoice`, specialized to Int since that is the most common type to want a choice as
makeChoiceInt :: Int -> ChoiceState Int
makeChoiceInt n = do
    !v <- makeChoice $ fromIntegral n
    pure $ fromIntegral v

-- | A weighted coin flip, which results in True with probability `p`, which should be
--   between 0 and 1. Shrinks to False.
weighted :: Double -> ChoiceState Bool
weighted !p
    | p >= 1 = forcedChoiceBool True
    | p <= 0 = forcedChoiceBool False
    | otherwise = do
          let fn :: Word64 -> Word64
              fn w = if w > w' then 1 else 0
                  where
                      d = fromRational $ toRational ((maxBound :: Word64) `div` 2)
                      w' = round $ d * (1-p)
          a <- makeChoiceFn (maxBound `div` 2) fn
          pure . toEnum $ fromIntegral a

-- | Make a "choice" that is always the value given. Having a choice represented in the
-- list seems to help shrinking, even if it is a fake one.
-- The internal representation of the forced choice can shrink, but this does not affect the value generated.
forcedChoice :: Int -> ChoiceState Int
forcedChoice !n = do
    let n' = fromIntegral n
    choice <- makeChoiceFn n' $ Relude.const n'
    pure $ fromIntegral choice

-- | forcedChoice, specialized for Bool because that is a common case that deserves ergonomics.
forcedChoiceBool :: Bool -> ChoiceState Bool
forcedChoiceBool !b = do
    let !b' = fromEnum b
    !c <- forcedChoice b'
    pure $ toEnum c

-- | Generate an Integral value. The range of possible values depends on the concrete type generated,
-- as it will match whatever has the same bit representation of a random Word64. For Int based types,
-- on x86-64 architectures, this will be +-2^63, or smaller for smaller Int types. Shrinks to 0.
integral :: Integral a => Gen a
integral = Gen $ do
    !a <- makeChoice maxBound
    pure $ fromIntegral a

-- | Generate an Integral value, as in `integral`, but instead of being between 0 and maxBound for Word64,
-- the underlying values will be between the values `lo` and `hi`. Shrinks to `lo`.
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

-- | `integralRange`, specialized to Int. Produces a value between `lo` and `hi`, shrinking to `lo`.
intRange :: Int -> Int -> Gen Int
intRange = integralRange

-- | Generates a list of `a`, with a random length that is geometrically distributed, average length 10.
-- Shrinks to shorter lists, and smaller values of `a`.
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

-- | Generates a list of `a`, with a length of at least `lo`, at most `hi`.
-- The length above `lo` is geometrically distributed with mean 10, capped at `hi` - `lo`.
-- TODO: Rewrite to make the average length of the list be the average of `lo` and `hi`.
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

-- | A generator that always returns the same value.
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

-- | Generate a random value in the full range of a bounded enum, with equal probability.
-- Shrinks to values with lower integer representation, which will be constructors defined earlier
-- for user types.
enumBounded :: (Enum a, Bounded a) => Gen a
enumBounded = enumRange minBound maxBound

-- | Generate a Bool, shrinking to False.
bool :: Gen Bool
bool = enumBounded

-- | Transform a generator to produce Maybe a, with equal probability of Just a or Nothing. Shrinks to Nothing.
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

-- | Generates a float. Float shrinking is probably the trickiest to implement of the primitives, because the desired shrink behavior is subject to a lot of constraints.
-- We want to shrink to positive, and for 0 to be the smallest value. We also want to shrink to 1 rather than to machine epsilon, because shrinking to very small values can mislead users into thinking they have a small rounding issue.
-- Ideally, this will shrink to positive values, with the exponent going to 0, and mantissa going to 0. Because shrinking starts from the end of the choice sequence, the shrunk value will favor having a 0 mantissa and slightly higher exponent over the alternative, even though the alternative is technically smaller in shortlex order.
float :: Gen Float
float = Gen $ do
    -- TODO: This really wants some tests
    exponent0 <- makeChoice (2 ^ (8 :: Int))
    mantissa0 <- makeChoice (2 ^ (23 :: Int))
    neg <- weighted 0.5
    let neg' = if neg then 1 else 0
        mantissa = fromIntegral mantissa0 :: Word32
        exponent = fromIntegral exponent0 :: Word32
        -- Swap the upper and lower halves of the byte range, so that 0 generates a 0 value
        -- exponent' = if exponent <= 128 then exponent + 127 else exponent - 128
        exponent' = exponent + 127
        neg32 = neg' `shiftL` 31
        w32 = neg32 .|. exponent' `shiftL` 23 .|. mantissa
        f = F.castWord32ToFloat w32
    -- trace ("neg " <> show neg <> "   mantissa " <> show mantissa <> "   exponent " <> show exponent'  <> "   " <> show w32 <> " -> " <> show f) $ pure f
    pure f

-- | Generator for Doubles. This should have the same shrinking behavior as float.
double :: Gen Double
double = Gen $ do
    -- TODO: This really wants some tests
    exponent0 <- makeChoice (2 ^ (11 :: Int))
    mantissa0 <- makeChoice (2 ^ (52 :: Int))
    neg <- weighted 0.5
    let neg' = if neg then 1 else 0
        mantissa = mantissa0 :: Word64
        exponent = exponent0 :: Word64
        -- Swap the upper and lower halves of the byte range, so that 0 generates a 0 value
        exponent' = exponent + 1023
        neg64 = neg' `shiftL` 63
        w64 = neg64 .|. exponent' `shiftL` 52 .|. mantissa
        f = F.castWord64ToDouble w64
    -- trace ("neg " <> show neg <> "   mantissa " <> show mantissa <> "   exponent " <> show exponent'  <> "   " <> show w32 <> " -> " <> show f) $ pure f
    pure f
