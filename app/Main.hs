module Main where

import Relude
import TestCase
import Examples

main :: IO ()
main = do
    printResult $ check example
    printResult $ check example2
    printResult $ check example3
    printResult $ check example4
    pure ()
