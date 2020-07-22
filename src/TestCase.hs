{-# LANGUAGE StrictData #-}
module TestCase where


import Relude hiding (splitAt, pred)
import qualified Relude.Unsafe as Unsafe
import qualified Data.List as L
import qualified System.Random as R

import Data.List.NonEmpty (splitAt)
import qualified Data.ByteString as B

import qualified Data.Tree as T


import Gen
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




(===) :: Eq a => a -> a -> Property a
a === b = do
    let r = if a == b
            then Valid
            else Interesting a b
    pure r



type Property a = Gen (PropertyResult a)

example :: Property [Int]
example = do
    l <- list int
    r <- list int
    l === r

type Test a = Property a -> PropertyResult a


check :: Show a => Int -> Property a -> IO (PropertyResult a)
check n cs = do
    -- g <- R.newStdGen
    let
        g = R.mkStdGen n
        gen = runChoiceState $ runGen cs
    let mRC = runStateT gen (Choices [] 0 (8 * 1024) g)
        -- (r, c) = fromMaybe (Invalid, Choices [] 0 (8*1024) g) mRC

    case mRC of
        Nothing -> print "invalid initial test"
        Just _ -> print "Valid initial test"
    let
        (r, c) = Unsafe.fromJust mRC
    print r
    let
        pred :: Choices -> Bool
        pred choice = res'
            where
                mRes = runStateT gen choice
                res' = case mRes of
                    Nothing -> False
                    Just (r2, c2) -> case r2 of
                        Interesting _ _ ->
                            case c2 of
                                Choices {unIndex=i, unMaxVal=v} | fromIntegral i > v -> False
                                _ -> True
                        _ -> False
        next = shrinkToFixpoint c pred
        mRC' = runStateT gen next

    case mRC' of
        Nothing -> print "invalid shrunk test"
        Just _ -> print "valid shrinking"
    let
        (r', c') = Unsafe.fromJust mRC'
    pure r'

check' :: Show a => Property a -> IO (PropertyResult a)
check' = check 0

check'' :: Show a => Property a -> IO (PropertyResult a)
check'' cs = do
    g <- R.newStdGen
    let n = fromIntegral $ fst $ R.genWord64 g
    let c = check n cs
    c


fromMaybeProp :: Maybe (PropertyResult a) -> PropertyResult a
fromMaybeProp = fromMaybe Invalid

printResult :: Show a => IO (PropertyResult a) -> IO ()
printResult res0 = do
    res <- res0
    let failureStr = " is not the same as " :: String
    case res of
        Valid -> putStrLn "Test passed"
        Interesting a b -> putStrLn $ show a <> failureStr <> show b
        Invalid -> putStrLn ""
    pure ()

data PropertyResult a = Valid
                      | Invalid
                      | Interesting a a
    deriving (Show)

