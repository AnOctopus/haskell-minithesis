module Main where

import Relude
import qualified Relude.Unsafe as Unsafe
import TestCase
import Examples

main :: IO ()
main = do
    printResult . pure $ Unsafe.head $ notable `filter`checkSome example [0..100]
    printResult . pure $ Unsafe.head $ notable `filter`checkSome example2 [0..100]
    printResult . pure $ Unsafe.head $ notable `filter`checkSome example3 [0..100]
    printResult . pure $ Unsafe.head $ notable `filter`checkSome example4 [0..100]
    pure ()
