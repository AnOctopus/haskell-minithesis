{-# LANGUAGE StrictData #-}
module TestCase where


import Relude hiding (splitAt)
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
            else Failure a b
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
        gen = runGen cs
    let (r, c) = runState gen (Choices [] 0 (8 * 1024) g)
        pred :: Choices -> Bool
        pred choices = res'
            where
                (res, _c2) = runState gen choices
                res' = case res of
                    Nothing -> False
                    Just (Failure _a _b) -> True
                    Just _ -> False
        next = deleteChunkPass c pred
        (r', c') = runState gen next
        r'' = fromMaybeProp r'

    pure r''

check' :: Show a => Property a -> IO (PropertyResult a)
check' = check 0

fromMaybeProp :: Maybe (PropertyResult a) -> PropertyResult a
fromMaybeProp = fromMaybe Invalid

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

