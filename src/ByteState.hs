{-# LANGUAGE StrictData #-}
module ByteState where


import Relude hiding (State, get, put, evalState, modify, splitAt, runState)
import qualified Relude (State, get, put, evalState, modify)
import qualified Relude.Unsafe as Unsafe
import qualified Data.List as L
import qualified System.Random as R

import Data.List.NonEmpty (splitAt)
import qualified Data.ByteString as B

import qualified Data.Random.Internal.Words as W

import Data.Bits

newtype Index = Index Natural
    deriving (Show, Eq, Ord, Num, Enum, Real, Integral)
newtype Size = Size Natural
    deriving (Show, Eq, Ord, Num, Enum, Real, Integral)

type Bytes = [Word8]
-- (a, remainingBytes)
newtype Gen a = Gen {runGen :: ByteString -> (a, ByteString)}

f1 :: (a -> b) -> (a, c) -> (b, c)
f1 f (a, c) = (f a, c)

f2 :: (a -> b) -> (a, c, d) -> (b, c, d)
f2 f (a, c, d) = (f a, c, d)

f3 :: (a -> b) -> (a, c, d, e) -> (b, c, d, e)
f3 f (a, c, d, e) = (f a, c, d, e)

instance Functor Gen where
    fmap :: (a -> b) -> Gen a -> Gen b
    fmap f (Gen a) = Gen $ f1 f . a

instance Applicative Gen where
    pure :: a -> Gen a
    pure a = Gen $ \bs -> (a, bs)

    (<*>) :: Gen (a -> b) -> Gen a -> Gen b
    Gen g1 <*> Gen g2 = Gen $ go g1 g2
        where
            go :: (ByteString -> (a -> b, ByteString)) -> (ByteString -> (a, ByteString)) -> ByteString -> (b, ByteString)
            go f ga bs = let
                (fn, rest) = f bs
                (val, rest2) = ga rest
                in (fn val, rest2)


genMany :: Gen a -> Gen [a]
genMany g = (:) <$> g <*> genMany g

genWord8 :: Gen Word8
genWord8 = Gen $ \(!bs) -> (B.head bs, B.tail bs)

genList :: Int -> Gen a -> Gen [a]
genList !size !g = take size <$> genMany g

-- | Replace index i in list l with value v
replace :: NonEmpty a -> Natural -> a -> NonEmpty a
replace l i v =
    let (h, t) = splitAt (fromIntegral (i - 1)) l
        r = Unsafe.tail h
    in
        case i of
            0 -> v :| tail l
            _ -> head l :| r <> [v] <> Unsafe.tail t

replace2 :: [a] -> Natural -> a -> [a]
replace2 !l !i !v =
    let (h, t) = L.splitAt (fromIntegral (i - 1)) l
        r = Unsafe.tail h
    in
        case i of
            0 -> v : Unsafe.tail l
            _ -> Unsafe.head l : r <> [v] <> Unsafe.tail t

(!!??) :: NonEmpty a -> Index -> Maybe a
l !!?? i = toList l !!? fromIntegral i


newtype MyState s g a = MyState {runState :: s -> g -> (a, s, g)}

instance Functor (MyState s g) where
    fmap :: (a -> b) -> MyState s g a -> MyState s g b
    fmap f (MyState a) = MyState g
        where
            fst3 (x, _y, _z) = x
            g s t= (f $ fst3 (a s t), s, t)

instance Applicative (MyState s g) where
    pure :: a -> MyState s g a
    pure a = MyState $ \s g -> (a, s, g)

    (<*>) :: MyState s g (a -> b) -> MyState s g a -> MyState s g b
    (MyState f) <*> (MyState a0) = MyState $ \s g -> go s g f a0
        where
            go s0 g0 fn fa = let
                -- call fn with s and t, to get the function ab and new state s1, t1
                (ab, s1, g1) = fn s0 g0
                -- call fa with s1, t1, to get value a and new states s2, t2
                (a, s2, g2) = fa s1 g1
                -- return the new states, along with the result of applying ab to a
                in (ab a, s2, g2)

instance Monad (MyState s g) where
    (>>=) :: MyState s g a -> (a -> MyState s g b) -> MyState s g b
    (!p) >>= (!f) = MyState $ \(!s) (!g) ->
        let (x, s1, g1) = runState p s g
        in runState (f x) s1 g1


evalMyState :: MyState s g a -> s -> g -> a
evalMyState st s g = a
    where
        (a, _, _) = runState st s g

type ByteStruct a = MyState ([ByteString], Index) R.StdGen a
newtype ByteState a = ByteState {unByteState :: ByteStruct a}
    deriving (Functor, Applicative, Monad)

runByteState :: Show a => ByteState a -> [ByteString] -> Index -> R.StdGen -> (a, ([ByteString], Index), R.StdGen)
-- runByteState bs idx gen byteState = runState (unByteState byteState) (bs, idx) gen
runByteState !byteState !bs !idx !gen = (a, b, c)
    where
        !byteStruct = unByteState byteState
        (!a, !b, !c) = runState byteStruct (bs, idx) gen

runByteStateIO :: ByteState a -> IO (a, ([ByteString], Index), R.StdGen)
runByteStateIO byteState = do
    gen <- R.newStdGen
    pure $ runState (unByteState byteState) ([], 0) gen

evalByteState :: [ByteString] -> Index -> R.StdGen -> ByteState a -> a
evalByteState bs idx gen byteState = a where
    (a, _, _) = runState (unByteState byteState) (bs, idx) gen

evalByteStateIO :: ByteState a -> IO a
evalByteStateIO byteState = do
    gen <- R.newStdGen
    let (a, _, _) = runState (unByteState byteState) ([], 0) gen
    pure a

get :: ByteState ([ByteString], Index)
get = ByteState . MyState $ \(!s) (!g) -> (s, s, g)

put :: ([ByteString], Index) -> ByteState ()
put !s = ByteState . MyState $ \_ g -> ((), s, g)

getByteList :: ByteState [ByteString]
getByteList = do
    (bs, _idx) <- get
    pure bs

putByteList :: [ByteString] -> ByteState ()
putByteList bytes = do
    (_, idx) <- get
    put (bytes, idx)

incIndex :: ByteState ()
incIndex = do
    (!bs, !idx) <- get
    put (bs, idx + 1)

getGen :: ByteState R.StdGen
getGen = ByteState . MyState $ \s g -> (g, s, g)

putGen :: R.StdGen -> ByteState ()
putGen !gen = ByteState . MyState $ \s _ -> ((), s, gen)

genBytes :: Size -> ByteState ByteString
genBytes !size = do
    g <- getGen
    let (!bytes, g') = R.genByteString (fromIntegral size) g
    putGen g'
    pure bytes

getOrCreateNext :: Size -> ByteState (ByteString, Index)
getOrCreateNext !size = do
    incIndex
    (!bytes, !idx) <- get
    let !v = case bytes !!? fromIntegral idx of
                Just !c -> pure (c, idx)
                Nothing -> do
                    (!newBytes) <- genBytes size
                    putByteList (bytes <> pure newBytes)
                    id $! pure (newBytes, idx)
    (!u) <- v
    put (bytes <> [fst u], snd u)
    v


putAt :: Index -> ByteString -> ByteState ()
putAt !idx !newBytes = do
    (byteState, i) <- get
    put (replace2 byteState (fromIntegral idx) newBytes, i)


-- | A `Property a` allows the generation of values and assertion of properties of `a`
newtype Property a = Property {runProperty :: ByteState a}
    deriving (Functor, Applicative, Monad)


-- (===) :: (Eq a, Show a) => a -> a -> Property (PropertyResult a)
-- -- a === b = Property . pure $ if a == b then Success else Failure a b
-- a === b = do
--     let r = a == b
--         r' = if r
--             then Success
--             else Failure a b
--     pure r'

(====) :: (Eq a, Show a) => a -> a -> ByteState (PropertyResult a)
(!a) ==== (!b) = do
    let r = if a == b
            then Success
            else Failure a b
    pure r

forAll :: Show a => Gen a -> ByteState a
forAll !gen = do
    (!bytes, !idx) <- getOrCreateNext 16000
    let (!a, ~rest) = runGen gen bytes
    putAt idx rest
    pure a


example :: ByteState (PropertyResult [Int])
example = do
    i <- forAll genWord8
    l <- forAll $! genList (fromIntegral i) genInt
    r <- forAll $! genList 3 genInt
    l ==== r

check :: Show a => ByteState (PropertyResult a) -> IO (PropertyResult a)
check !bs = do
    g <- R.newStdGen
    let
        (!r, (!st, _i), !g') = runByteState bs [] 0 g
    let
        (!r2, (!s2, i2), g2) = runByteState bs (shrinkAllZero <$> st) 0 g'

    print r
    pure r2

printResult :: Show a => IO (PropertyResult a) -> IO ()
printResult res = do
    foo <- res
    let failureStr = " is not the same as " :: String
    case foo of
        Success -> putStrLn "Test passed"
        Failure a b -> putStrLn $ show a <> failureStr <> show b
    pure ()

data PropertyResult a = Success
                      | Failure a a
    deriving (Show)

shrinkAllZero :: ByteString -> ByteString
shrinkAllZero bs = B.pack b'
    where
        bytes = B.unpack bs
        b' = (0 .&.) <$> bytes


shrinkZeroFirst :: ByteString -> ByteString
shrinkZeroFirst bs = B.pack $ zero : Unsafe.tail bytes
    where
        bytes = B.unpack bs
        zero = 0 :: Word8

shrinkZeroFirst' :: [ByteString] -> [ByteString]
shrinkZeroFirst' bs = newBytes : Unsafe.tail bs
    where
        firstBS = Unsafe.head bs
        newBytes = shrinkZeroFirst firstBS

genInt :: Gen Int
genInt = Gen $ \(!bs) -> f bs
    where
        f :: ByteString -> (Int, ByteString)
        f bs = (fromIntegral word, B.pack rest)
            where
                uBs = B.unpack bs
                w64 = take 8 uBs
                -- (a:b:c:d:e:f:g:h:_) = trace (show w64) $ w64
                (a:b:c:d:e:f:g:h:_) = w64
                rest = drop 8 uBs
                word = W.buildWord64 a b c d e f g h
