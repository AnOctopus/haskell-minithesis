module Internal.Util where

import Relude hiding ((<**>))


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

(-.) :: Natural -> Natural -> Natural
a -. b = if b >= a then 0 else a - b
