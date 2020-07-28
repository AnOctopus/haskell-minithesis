{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
module TestCase where


import Relude hiding (splitAt, pred)
import qualified Relude.Unsafe as Unsafe
import qualified Data.List as L
import qualified System.Random as R

import Data.List.NonEmpty (splitAt)
import qualified Data.ByteString as B

import qualified Data.Tree as T
import qualified Data.Vector.Unboxed as V


import Gen
import Shrink

data PropertyResult a = Valid
                      | Invalid
                      | Interesting a a
                      | AssertFailure String
    deriving (Show)


(===) :: Eq a => a -> a -> Property a
a === b = do
    let r = if a == b
            then Valid
            else Interesting a b
    pure r

(/==) :: Eq a => a -> a -> Property a
a /== b = do
    let r = if a /= b
            then Valid
            else Interesting a b
    pure r


example :: Property [Int]
example = do
    l <- listRange 0 2000 int
    r <- list int
    l === r

-- TODO: This test rarely fails because `list` doesn't produce empty list often enough
-- which is an instance of the general problem of not producing enough small values
example2 :: Property [Int]
example2 = do
    l <- list int
    let r = []
    l /== r

example3 :: Property Bool
example3 = do
    i <- intRange 0 100
    lst <- listRange i (i*2) int
    assert (not (any (>=9) lst)) lst


type Property a = Gen (PropertyResult a)
type Test a = Property a -> PropertyResult a

diff :: a -> (a -> b -> Bool) -> b -> Property Bool
diff = undefined


assert :: Show a => Bool -> a -> Property Bool
assert b msg = Gen . pure $ if b then Valid else AssertFailure $ show msg

pred :: StateT Choices Maybe (PropertyResult a) -> Choices -> Bool
pred test choice = res'
    where
        mRes = runStateT test choice
        res' = case mRes of
            Nothing -> False
            Just (r2, c2) -> case r2 of
                Interesting _ _ ->
                    case c2 of
                        Choices {unIndex=i, unMaxVal=v}
                            | fromIntegral i > v -> False
                            | unBytes c2 > unBytes choice -> False
                            | otherwise -> True
                AssertFailure _ -> True
                _ -> False

checkOne :: Int -> Property a -> IO (PropertyResult a)
checkOne n cs = do
    let
        g = R.mkStdGen n
        test = runChoiceState $ runGen cs
        mRC = runStateT test (Choices V.empty 0 (8 * 1024) g)
    case mRC of
        Nothing -> print "Invalid/rejected initial test"
        -- Just _ -> print $ "Valid initial test from seed=" <> show n
        Just _ -> pure ()
    let r = fromMaybeProp (fst <$> mRC)
        r' = if interesting r then do
            let
                c = Unsafe.fromJust (snd <$> mRC)
                next = shrinkToFixpoint c $ pred test
                mRC' = runStateT test next
            fromMaybeProp (fst <$> mRC')
            else r

    pure r'

interesting :: PropertyResult a -> Bool
interesting = \case
    Interesting _ _ -> True
    AssertFailure _ -> True
    _ -> False

checkOneR :: Property a -> IO (PropertyResult a, Int)
checkOneR cs = do
    g <- R.newStdGen
    let n = fromIntegral $ fst $ R.genWord64 g
    res <- checkOne n cs
    pure (res, n)

check :: Property a -> IO (PropertyResult a)
check cs = do
    tests <- replicateM 100 $ checkOneR cs
    let failed = (interesting . fst) `filter` tests
        firstFailed = if not (null failed) then Unsafe.head failed else Unsafe.head tests
    print (snd firstFailed)
    pure (fst firstFailed)

fromMaybeProp :: Maybe (PropertyResult a) -> PropertyResult a
fromMaybeProp = fromMaybe Invalid

printResult :: Show a => IO (PropertyResult a) -> IO ()
printResult res0 = do
    res <- res0
    let failureStr = " is not the same as " :: String
    case res of
        Valid -> putStrLn "Test passed"
        Interesting a b -> putStrLn $ show a <> failureStr <> show b
        AssertFailure s -> putStrLn $ "Assertion invalidated by " <> show s
        Invalid -> putStrLn ""
    pure ()

