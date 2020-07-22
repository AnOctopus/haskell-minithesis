module Gen where

import Relude hiding ((<**>))

import qualified Data.Random.Internal.Words as W
import Data.Ratio
import qualified Data.Tree as T
import qualified Relude.Unsafe as Unsafe
import qualified System.Random as R

newtype Index = Index Natural
    deriving (Show, Eq, Ord, Num, Enum, Real, Integral)
newtype Size = Size Natural
    deriving (Show, Eq, Ord, Num, Enum, Real, Integral)


f1 :: (a -> b) -> (a, c) -> (b, c)
f1 f (a, c) = (f a, c)

(<$$>) :: (Functor f, Functor g) => (a -> b) -> f (g a) -> f (g b)
h <$$> m = fmap h <$> m
infixl 4 <$$>

(<**>) :: (Applicative f, Applicative g) => f (g (a -> b)) -> f (g a) -> f (g b)
h <**> m = liftA2 (<*>) h m
infixl 4 <**>

newtype ChoiceState a = ChoiceState {runChoiceState :: StateT Choices Maybe a}
    deriving newtype (Functor, Applicative, Monad, MonadState Choices, MonadFail)
-- data Choices = Choices [Word64] Index Natural R.StdGen
data Choices = Choices {unBytes :: [Word64], unIndex :: Index, unMaxVal :: Natural, unGen :: R.StdGen}

choices :: [Word64] -> Natural -> R.StdGen -> Choices
choices bytes maxVal gen = Choices bytes 0 maxVal gen

resetIndex :: Choices -> Choices
resetIndex (Choices w _idx maxVal g) = Choices w 0 maxVal g

-- Choices -> (Maybe a, Choices)
newtype Gen a = Gen {runGen :: ChoiceState a}
    deriving (Functor, Applicative, Monad)


makeChoice :: Word64 -> ChoiceState Word64
makeChoice n = do
    (Choices !bytes !idx !maxValue !stdgen) <- get
    let
        i = fromIntegral idx
        (b, stdgen') = if i < length bytes
            then (bytes Unsafe.!! i, stdgen)
            else R.genWord64R n stdgen
    -- trace ("bytes=" <> show bytes <> " idx=" <> show idx <> " length=" <> show (length bytes) <> " maxValue=" <> show maxValue) $
    put $ Choices (bytes <> pure b) (idx + 1) maxValue stdgen'
    if (idx + 1) > fromIntegral maxValue
       then trace "data overrun" $ fail "Data overrun"
       else pure b

weighted :: Double -> ChoiceState Bool
weighted p = do
    a <- makeChoice 100
    let i = fromIntegral a :: Integer
        res = fromRational (i % 100) < p
    pure res


int :: Gen Int
int = Gen f where
    f = do
        a <- makeChoice maxBound
        pure $ fromIntegral a

list :: forall a. Gen a -> Gen [a]
list gen = Gen $ do
    b <- weighted 0.9
    case b of
        -- stop here, return an empty list. the only stateful computation is bool choice
        False -> pure []
        -- generate an element, then append it to the results of calling list again
        True -> do
            newVal <- runGen gen
            let nextList = runGen $ list gen
            l' <- nextList
            let newList = l' <> pure newVal
            pure newList
