{-# LANGUAGE StrictData #-}
module TestCase where


import Relude hiding (State, get, put, evalState, modify, splitAt, runState)
import qualified Relude (State, get, put, evalState, modify)
import qualified Relude.Unsafe as Unsafe
import qualified Data.List as L
import qualified System.Random as R

import Data.List.NonEmpty (splitAt)
import qualified Data.ByteString as B

import qualified Data.Tree as T


import Gen
import State
import Shrink



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

type ByteStruct a = MyState (Choices, Index) R.StdGen a
newtype TestCase a = TestCase {unTestCase :: ByteStruct a}
    deriving (Functor, Applicative, Monad)

runTestCase :: TestCase a -> Choices -> Index -> R.StdGen -> (a, (Choices, Index), R.StdGen)
runTestCase byteState bs idx gen = (a, b, c)
    where
        byteStruct = unTestCase byteState
        (a, b, c) = runState byteStruct (bs, idx) gen

-- runTestCaseIO :: TestCase a -> IO (a, (Choices, Index), R.StdGen)
-- runTestCaseIO byteState = do
--     gen <- R.newStdGen
--     pure $ runState (unTestCase byteState) ([], 0) gen

evalTestCase :: Choices -> Index -> R.StdGen -> TestCase a -> a
evalTestCase bs idx gen byteState = a where
    (a, _, _) = runState (unTestCase byteState) (bs, idx) gen

-- evalTestCaseIO :: TestCase a -> IO a
-- evalTestCaseIO byteState = do
--     gen <- R.newStdGen
--     let (a, _, _) = runState (unTestCase byteState) ([], 0) gen
--     pure a

get :: TestCase (Choices, Index)
get = TestCase . MyState $ \s g -> (s, s, g)

put :: (Choices, Index) -> TestCase ()
put s = TestCase . MyState $ \_ g -> ((), s, g)

getByteList :: TestCase Choices
getByteList = do
    (bs, _idx) <- get
    pure bs

putByteList :: Choices -> TestCase ()
putByteList bytes = do
    (_, idx) <- get
    put (bytes, idx)

incIndex :: TestCase ()
incIndex = do
    (bs, idx) <- get
    put (bs, idx + 1)

getGen :: TestCase R.StdGen
getGen = TestCase . MyState $ \s g -> (g, s, g)

putGen :: R.StdGen -> TestCase ()
putGen gen = TestCase . MyState $ \s _ -> ((), s, gen)

-- genBytes :: Size -> TestCase Choices
-- genBytes size = do
--     g <- getGen
--     let (bytes, g') = R.genByteString (fromIntegral size) g
--     putGen g'
--     pure $ B.unpack bytes

-- getOrCreateNext :: Size -> TestCase (Choice, Index)
-- getOrCreateNext size = do
--     incIndex
--     (bytes, idx) <- get
--     let v = case bytes !!? fromIntegral idx of
--                 Just c -> pure (c, idx)
--                 Nothing -> do
--                     newBytes <- genBytes size
--                     let new = T.Node newBytes []
--                     putByteList (bytes <> pure new)
--                     pure (new, idx)
--     u <- v
--     put (bytes <> [fst u], snd u)
--     v

-- putAt :: Index -> Choice -> TestCase ()
-- putAt idx newBytes = do
--     (byteState, i) <- get
--     put (replace2 byteState (fromIntegral idx) newBytes, i)


(===) :: Eq a => a -> a -> TestCase (PropertyResult a)
a === b = do
    let r = if a == b
            then Valid
            else Failure a b
    pure r

-- Gen a -> Maybe ([Bytes], Index) a
-- forAll :: Gen a -> Maybe (TestCase a)
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
-- forAll :: Gen a -> TestCase (Maybe a)
-- forAll gen = do
--     (bytes, idx) <- getOrCreateNext 16000
--     let mA = runGen gen bytes
--     case mA of
--         Nothing -> pure Nothing
--         Just (a, r) -> do
--             putAt idx r
--             pure $ Just a

-- forAll' :: Gen a -> TestCase a
-- forAll' gen = do
--     (bytes, idx) <- getOrCreateNext 16000
--     let (a, rest) = Unsafe.fromJust $ runGen gen bytes
--     putAt idx rest
--     pure a

-- example :: TestCase (PropertyResult [Int])
-- example = do
--     i <- forAll' genWord8
--     l <- forAll' $ genList (fromIntegral i) genInt
--     r <- forAll' $ genList 2 genInt
--     l === r

-- check :: Show a => TestCase (PropertyResult a) -> IO (PropertyResult a)
-- check bs = do
--     -- g <- R.newStdGen
--     let
--         g = R.mkStdGen 0
--         (r, (st, _i), g') = runTestCase bs [] 0 g
--         (r2, (_s2, _i2), _g2) = runTestCase bs (shrinkAllZero <$> st) 0 g'

--     print r
--     pure r2
--     -- pure r

printResult :: Show a => IO (PropertyResult a) -> IO ()
printResult res0 = do
    res <- res0
    let failureStr = " is not the same as " :: String
    case res of
        Valid -> putStrLn "Test passed"
        Failure a b -> putStrLn $ show a <> failureStr <> show b
        Invalid -> putStrLn ""
    pure ()

data PropertyResult a = Valid
                      | Invalid
                      | Failure a a
    deriving (Show)

