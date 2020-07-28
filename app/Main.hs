module Main where

import Relude
import TestCase

main :: IO ()
main = do
    printResult $ check example
    printResult $ check example2
    printResult $ check example3
    pure ()
