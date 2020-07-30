module Main where

import Relude
import TestCase

main :: IO ()
main = do
    printResult $ checkOne 0 example
    printResult $ checkOne 1 example2
    printResult $ checkOne 2 example3
    pure ()
