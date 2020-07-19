{-# LANGUAGE StrictData #-}
module ByteState where


import Relude hiding (State, get, put, evalState, modify, splitAt, runState)
import qualified Relude (State, get, put, evalState, modify)
import qualified Relude.Unsafe as Unsafe
import qualified Data.List as L
import qualified System.Random as R

import Data.List.NonEmpty (splitAt)
import qualified Data.ByteString as B



import Gen
import State
import Shrink

newtype Index = Index Natural
    deriving (Show, Eq, Ord, Num, Enum, Real, Integral)
newtype Size = Size Natural
    deriving (Show, Eq, Ord, Num, Enum, Real, Integral)


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

type ByteStruct a = MyState ([Bytes], Index) R.StdGen a
newtype ByteState a = ByteState {unByteState :: ByteStruct a}
    deriving (Functor, Applicative, Monad)

runByteState :: ByteState a -> [Bytes] -> Index -> R.StdGen -> (a, ([Bytes], Index), R.StdGen)
runByteState !byteState !bs !idx !gen = (a, b, c)
    where
        !byteStruct = unByteState byteState
        (!a, !b, !c) = runState byteStruct (bs, idx) gen

runByteStateIO :: ByteState a -> IO (a, ([Bytes], Index), R.StdGen)
runByteStateIO byteState = do
    gen <- R.newStdGen
    pure $ runState (unByteState byteState) ([], 0) gen

evalByteState :: [Bytes] -> Index -> R.StdGen -> ByteState a -> a
evalByteState bs idx gen byteState = a where
    (a, _, _) = runState (unByteState byteState) (bs, idx) gen

evalByteStateIO :: ByteState a -> IO a
evalByteStateIO byteState = do
    gen <- R.newStdGen
    let (a, _, _) = runState (unByteState byteState) ([], 0) gen
    pure a

get :: ByteState ([Bytes], Index)
get = ByteState . MyState $ \(!s) (!g) -> (s, s, g)

put :: ([Bytes], Index) -> ByteState ()
put !s = ByteState . MyState $ \_ g -> ((), s, g)

getByteList :: ByteState [Bytes]
getByteList = do
    (bs, _idx) <- get
    pure bs

putByteList :: [Bytes] -> ByteState ()
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

genBytes :: Size -> ByteState Bytes
genBytes !size = do
    g <- getGen
    let (!bytes, g') = R.genByteString (fromIntegral size) g
    putGen g'
    pure $ B.unpack bytes

getOrCreateNext :: Size -> ByteState (Bytes, Index)
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

putAt :: Index -> Bytes -> ByteState ()
putAt !idx !newBytes = do
    (byteState, i) <- get
    put (replace2 byteState (fromIntegral idx) newBytes, i)


(===) :: Eq a => a -> a -> ByteState (PropertyResult a)
(!a) === (!b) = do
    let r = if a == b
            then Success
            else Failure a b
    pure r

-- Gen a -> Maybe ([Bytes], Index) a
-- forAll :: Gen a -> Maybe (ByteState a)
-- forAll gen = do
--     let foo = do
--             (bytes, idx) <- getOrCreateNext 16000
--             let mA = case runGen gen bytes of
--                     Nothing -> Nothing
--                     Just (a, r) -> Just $ do
--                         putAt idx r
--                         pure a
--             sequence mA
--     undefined

-- Gen a -> State ([Bytes], Index) (Maybe a)
forAll :: Gen a -> ByteState (Maybe a)
forAll gen = do
    (bytes, idx) <- getOrCreateNext 16000
    let mA = runGen gen bytes
    case mA of
        Nothing -> pure Nothing
        Just (a, r) -> do
            putAt idx r
            pure $ Just a

forAll' :: Gen a -> ByteState a
forAll' !gen = do
    (!bytes, !idx) <- getOrCreateNext 16000
    let (!a, ~rest) = Unsafe.fromJust $ runGen gen bytes
    putAt idx rest
    pure a

example :: ByteState (PropertyResult [Int])
example = do
    i <- forAll' genWord8
    l <- forAll' $! genList (fromIntegral i) genInt
    r <- forAll' $! genList 2 genInt
    l === r

check :: Show a => ByteState (PropertyResult a) -> IO (PropertyResult a)
check bs = do
    -- g <- R.newStdGen
    let
        g = R.mkStdGen 0
        (r, (st, _i), g') = runByteState bs [] 0 g
        (!r2, (!_s2, _i2), _g2) = runByteState bs (shrinkAllZero <$> st) 0 g'

    print r
    -- pure r2
    pure r

printResult :: Show a => IO (PropertyResult a) -> IO ()
printResult res0 = do
    res <- res0
    let failureStr = " is not the same as " :: String
    case res of
        Success -> putStrLn "Test passed"
        Failure a b -> putStrLn $ show a <> failureStr <> show b
    pure ()

data PropertyResult a = Success
                      | Failure a a
    deriving (Show)


