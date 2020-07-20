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


data Choices = Choices [Word64] Index R.StdGen
type ChoiceState a = State Choices a

-- Choices -> (Maybe a, Choices)
newtype Gen a = Gen {runGen :: ChoiceState (Maybe a)}

instance Functor Gen where
    fmap f (Gen a) = Gen $ f <$$> a

instance Applicative Gen where
    pure a = Gen . pure $ Just a

    Gen g1 <*> Gen g2 = Gen $ g1 <**> g2


makeChoice :: Word64 -> ChoiceState Word64
makeChoice n = do
    (Choices bytes idx stdgen) <- get
    let i = fromIntegral idx
        (b, stdgen') = if i < length bytes
            then (bytes Unsafe.!! i, stdgen)
            else R.genWord64R n stdgen
    put $ Choices (bytes <> pure b) (idx + 1) stdgen'
    pure b

weighted :: Double -> ChoiceState Bool
weighted p = do
    a <- makeChoice 100
    let i = fromIntegral a :: Integer
    pure $ fromRational (i % 100) < p


int :: Gen Int
int = Gen f where
    f = do
        a <- makeChoice maxBound
        pure . Just $ fromIntegral a

list :: forall a. Gen a -> Gen [a]
list gen = Gen $ do
    b <- weighted 0.9
    case b of
        -- stop here, return an empty list. the only stateful computation is bool choice
        False -> pure $ Just []
        -- generate an element, then append it to the results of calling list again
        True -> do
            newVal <- runGen gen
            let nextList = runGen $ list gen
            l' <- nextList
            let newList = liftA2 (<>) l' ((:[]) <$> newVal)
            pure newList

-- instance Alternative Gen where
--     empty :: Gen a
--     empty = Gen $ const Nothing

--     (<|>) :: Gen a -> Gen a -> Gen a
--     (Gen g1) <|> (Gen g2) = Gen $ go g1 g2
--         where
--             -- go :: (Bytes -> Maybe (a, Bytes)) -> (Bytes -> Maybe (a, Bytes)) -> (Bytes -> Maybe (a, Bytes))
--             go p1 p2 str = p1 str <|> p2 str
