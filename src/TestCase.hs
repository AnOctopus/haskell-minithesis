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
import qualified Data.Vector.Storable as V


import Gen
import Shrink

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

example2 :: Property [Int]
example2 = do
    l <- list int
    let r = [0, 0]
    l === r

type Test a = Property a -> PropertyResult a


check :: Show a => Int -> Property a -> IO (PropertyResult a)
check n cs = do
    -- g <- R.newStdGen
    let
        g = R.mkStdGen n
        gen = runChoiceState $ runGen cs
    let mRC = runStateT gen (Choices V.empty 0 (8 * 1024) g)
        -- (r, c) = fromMaybe (Invalid, Choices [] 0 (8*1024) g) mRC

    case mRC of
        Nothing -> print "invalid initial test"
        Just _ -> print $ "Valid initial test from seed=" <> show n
    let
        (_r, c) = Unsafe.fromJust mRC
    -- print r
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
        (r', _c') = Unsafe.fromJust mRC'
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

