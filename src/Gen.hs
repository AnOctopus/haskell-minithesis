{-# LANGUAGE StrictData #-}
module Gen where

import Relude
import qualified Data.Random.Internal.Words as W

type Bytes = [Word8]

-- (a, remainingBytes)
newtype Gen a = Gen {runGen :: Bytes -> Maybe (a, Bytes)}

f1 :: (a -> b) -> (a, c) -> (b, c)
f1 f (a, c) = (f a, c)

instance Functor Gen where
    fmap f (Gen a) = Gen $ fmap (f1 f) . a

instance Applicative Gen where
    pure a = Gen $ \bs -> Just (a, bs)

    Gen g1 <*> Gen g2 = Gen $ go g1 g2
        where
            go :: (Bytes -> Maybe (a -> b, Bytes)) -> (Bytes -> Maybe (a, Bytes)) -> Bytes -> Maybe (b, Bytes)
            go f ga bs =
                case f bs of
                    Nothing -> Nothing
                    Just (fn, rest) ->
                        case ga rest of
                            Nothing -> Nothing
                            Just (paVal, remainder) -> Just (fn paVal, remainder)

instance Alternative Gen where
    empty :: Gen a
    empty = Gen $ const Nothing

    (<|>) :: Gen a -> Gen a -> Gen a
    (Gen g1) <|> (Gen g2) = Gen $ go g1 g2
        where
            go :: (Bytes -> Maybe (a, Bytes)) -> (Bytes -> Maybe (a, Bytes)) -> (Bytes -> Maybe (a, Bytes))
            go p1 p2 str = p1 str <|> p2 str

genMany :: Gen a -> Gen [a]
genMany g = (:) <$> g <*> (genMany g <|> pure [])

genList :: Natural -> Gen a -> Gen [a]
genList size gen = take (fromIntegral size) <$> genMany gen

genWord8 :: Gen Word8
genWord8 = Gen $ \bs -> (,) <$> viaNonEmpty head bs <*> viaNonEmpty tail bs

genInt :: Gen Int
genInt = Gen  fn
    where
        fn :: Bytes -> Maybe (Int, Bytes)
        fn uBs = foo
            where
                w64 = take 8 uBs
                rest = drop 8 uBs
                foo = case w64 of
                    (a:b:c:d:e:f:g:h:_) -> Just (fromIntegral $ W.buildWord64 a b c d e f g h, rest)
                    _ -> Nothing
