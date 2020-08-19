module Examples where

import Relude

import TestCase
import Gen

example :: Property [Int]
example = do
    l <- listRange 0 2000 int
    r <- list int
    l === r

example2 :: Property [Int]
example2 = do
    l <- list int
    l /== []

example3 :: Property Bool
example3 = do
    i <- intRange 0 100
    lst <- listRange i (i*2) int
    assert (not (any (>=9) lst)) lst

example4 :: Property Bool
example4 = do
    f <- float
    assert (abs f <= 1.0) f
