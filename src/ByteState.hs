module ByteState where


{-
What do I need to implement this (MVP)?

A type representing the underlying bytestream. Can I just use ByteString and Random's genByteString?
    This will ultimately need to hang on to the values it produces, but that seems taken care of by keeping it as a state in the first place
A generator function `ByteString -> Maybe a` that produces values using the random data
    Rely on `read`, because I don't think we can implement this ourselves
A shrink function `ByteString -> ByteString` that tries to reduce the bytes to something simpler that still results in values that cause the test to fail

How do I rerun the tests with the new bytestring?
    This is done by keeping the state and calling runState with new values
How do I run the tests, then roll back if the changes don't work?
    As above
How do I maintain the bytestring for each generator as separate?
    Fake it with a list of them, for now
-}

import Relude hiding (State, get, put, evalState, modify, splitAt, runState)
import qualified Relude (State, get, put, evalState, modify)
import qualified Relude.Unsafe as Unsafe
import qualified Data.List as L
import qualified System.Random as R

import Data.List.NonEmpty (splitAt)
import qualified Data.ByteString as B

import System.IO.Unsafe (unsafePerformIO)

-- import Control.Monad.Freer
-- import Control.Monad.Freer.TH
-- import Control.Monad.Freer.State
-- import Control.Monad.Freer.Random
-- import Control.Monad.Freer.Internal hiding ((:|))


{-
genValue idx type = do
    bytes <- getOrCreate idx
    return type `parseFrom` bytes

tryShrinkValue idx type = do
    bytes <- get idx
    return tryRandomShrink bytes

storeSuccessfulShrink idx bytes = do
    put idx bytes
-}


-- data RandomBSGen a where
--     GenByteString :: Natural -> RandomBSGen ByteString

-- data MyState r where
--     GetOrCreate :: Index -> Natural -> MyState ByteString
--     Update :: Index -> ByteString -> MyState ()

-- makeEffect ''MyState
-- makeEffect ''RandomBSGen

-- runRandomWithSeed :: Int -> Eff (RandomBSGen ': effs) v -> Eff effs v
-- runRandomWithSeed seed = handleRelayS (R.mkStdGen seed) (\_gen val -> return val) handler
--   where
--     handler :: R.StdGen -> RandomBSGen a -> (R.StdGen -> a -> Eff r b) -> Eff r b
--     handler gen (GenByteString size) next =
--         let r = R.genByteString (fromIntegral size) gen in
--             next (snd r) (fst r)


-- runMyState :: Eff '[MyState, effs] ~> Eff '[State [ByteString], RandomBSGen, effs]
-- runMyState = reinterpret2 $ \case
--     GetOrCreate idx size -> do
--         st <- get
--         let val = case st !!? (fromIntegral idx) of
--                 Just contents -> pure contents
--                 Nothing -> do
--                     newVal <- genByteString size
--                     modify (<> [newVal])
--                     pure newVal
--         val
--     Update idx val -> do
--         st <- get
--         put $ replace2 st idx val


newtype Index = Index Natural
    deriving (Show, Eq, Ord, Num, Enum, Real, Integral)
newtype Size = Size Natural
    deriving (Show, Eq, Ord, Num, Enum, Real, Integral)

-- type Index = Natural
-- type Size = Natural

-- (a, remainingBytes, bytesConsumed)
newtype Gen a = Gen {runGen :: ByteString -> (a, ByteString, Natural)}

-- class GenM a

-- instance GenM (Gen a)

f1 :: (a -> b) -> (a, c) -> (b, c)
f1 f (a, c) = (f a, c)

f2 :: (a -> b) -> (a, c, d) -> (b, c, d)
f2 f (a, c, d) = (f a, c, d)

f3 :: (a -> b) -> (a, c, d, e) -> (b, c, d, e)
f3 f (a, c, d, e) = (f a, c, d, e)

instance Functor Gen where
    fmap :: (a -> b) -> Gen a -> Gen b
    fmap f (Gen a) = Gen $ f2 f . a

instance Applicative Gen where
    pure :: a -> Gen a
    pure a = Gen $ \bs -> (a, bs, 0)

    (<*>) :: Gen (a -> b) -> Gen a -> Gen b
    Gen g1 <*> Gen g2 = Gen $ go g1 g2
        where
            go :: (ByteString -> (a -> b, ByteString, Natural)) -> (ByteString -> (a, ByteString, Natural)) -> ByteString -> (b, ByteString, Natural)
            go f ga bs = let
                (fn, rest, s1) = f bs
                (val, rest2, s2) = ga rest
                in (fn val, rest2, s1 + s2)


genMany :: Gen a -> Gen [a]
genMany g = (:) <$> g <*> genMany g


genWord8 :: Gen Word8
genWord8 = Gen $ \bs -> (B.head bs, B.tail bs, 1)

genList :: Int -> Gen a -> Gen [a]
genList size g = take size <$> genMany g


shrinkBytes :: ByteString -> ByteString
shrinkBytes = id

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
replace2 l i v =
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
    p >>= f = r
        where
            p' = runState p
            f' = runState . f
            q s g = (y, s2, g2) where
                (x, s1, g1) = p' s g
                (y, s2, g2) = f' x s1 g1
            r = MyState q

get :: MyState s g s
get = MyState $ \s g -> (s, s, g)

put :: s -> MyState s g ()
put s = MyState $ \_ g -> ((), s, g)

modify :: (s -> s) -> MyState s g ()
modify f = get >>= \x -> put (f x)

evalMyState :: MyState s g a -> s -> g -> a
evalMyState st s g = a
    where
        (a, _, _) = runState st s g

type ByteState a = MyState ([ByteString], Index) R.StdGen a
-- newtype ByteState a = ByteState {runByteState :: Bytes a}
--     deriving (Functor, Applicative, Monad)

runByteState :: [ByteString] -> Index -> R.StdGen -> ByteState a -> a
runByteState bs idx gen byteState = a where
    (a, _, _) = runState byteState (bs, idx) gen

runByteStateIO :: ByteState a -> IO a
runByteStateIO byteState = do
    gen <- R.newStdGen
    let (a, _, _) = runState byteState ([], 0) gen
    pure a

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
    (bs, idx) <- get
    put (bs, idx + 1)

getGen :: MyState s g g
getGen = MyState $ \s g -> (g, s, g)

putGen :: g -> MyState s g ()
putGen gen = MyState $ \s _ -> ((), s, gen)

genBytes :: Size -> ByteState ByteString
genBytes size = do
    g <- getGen
    let (bytes, g') = R.genByteString (fromIntegral size) g
    putGen g'
    pure bytes

getOrCreateNext :: Size -> ByteState (ByteString, Index)
getOrCreateNext size = do
    incIndex
    (bytes, idx) <- get
    let v = case bytes !!? fromIntegral idx of
                Just c -> pure (c, idx)
                Nothing -> do
                    newBytes <- genBytes size
                    putByteList (bytes <> pure newBytes)
                    pure (newBytes, idx)
    v

getOrCreateSpecific :: Size -> Index -> ByteState ByteString
getOrCreateSpecific size idx = do
    bytes <- getByteList
    let v = case bytes !!? fromIntegral idx of
                Just c -> pure c
                Nothing -> do  -- We indexed off the list, so we just have to append
                    newBytes <- genBytes size
                    putByteList (bytes <> pure newBytes)
                    pure newBytes
    v


foo :: Gen a -> ByteState a
foo gen = do
    (bytes, idx) <- getOrCreateNext 100
    let (a, rest, consumed) = runGen gen bytes
    putAt idx rest
    pure a

putAt :: Index -> ByteString -> ByteState ()
putAt idx newBytes = do
    (byteState, i) <- get
    put (replace2 byteState (fromIntegral idx) newBytes, i)


-- | A `Property a` allows the generation of values and assertion of properties of `a`
newtype Property a = Property {runProperty :: ByteState a}
    deriving (Functor, Applicative, Monad)

(===) :: Eq a => a -> a -> Property Bool
a === b = Property . pure $ a == b

forAll :: (Show a) => Gen a -> Property a
forAll gen = do
    (bytes, _i) <- Property $ getOrCreateNext 100
    let (a, _, _) = runGen gen bytes
    return a

example :: Property Bool
example = do
    l <- forAll $ genList 10 genWord8
    l === [111, 222]
    -- pure undefined

check :: Property a -> IO a
check prop = result
    where
        bs = runProperty prop
        result = runByteStateIO bs
