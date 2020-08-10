module Internal.Util where

import Relude hiding ((<**>))

f1 :: (a -> b) -> (a, c) -> (b, c)
f1 f (a, c) = (f a, c)

(<$$>) :: (Functor f, Functor g) => (a -> b) -> f (g a) -> f (g b)
h <$$> m = fmap h <$> m
infixl 4 <$$>

(<**>) :: (Applicative f, Applicative g) => f (g (a -> b)) -> f (g a) -> f (g b)
h <**> m = liftA2 (<*>) h m
infixl 4 <**>

newtype Index = Index Natural
    deriving (Show, Eq, Ord, Num, Enum, Real, Integral, Generic)
instance NFData Index

newtype Size = Size Natural
    deriving (Show, Eq, Ord, Num, Enum, Real, Integral)
