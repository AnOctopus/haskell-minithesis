import Relude
import qualified Relude.Unsafe as Unsafe

import Test.Hspec hiding (example)

import TestCase
import Examples
import Gen


main :: IO ()
main = hspec $ do
    describe "TestCases" $ do
        it "finds counterexample in example 1" $ do
            let failure = Unsafe.head $ notable `filter` checkSome example [0..100]
            failure `shouldBe` Failure [0] []
        it "finds counterexample in example 2" $ do
            let failure = Unsafe.head $ notable `filter` checkSome example2 [0..100]
            failure `shouldBe` Failure [] []
        it "finds counterexample in example 3" $ do
            let failure = Unsafe.head $ notable `filter` checkSome example3 [0..100]
            failure `shouldBe` AssertFailure "[9]"
        it "finds counterexample in example 4" $ do
            let failure = Unsafe.head $ notable `filter` checkSome example4 [0..100]
            failure `shouldBe` AssertFailure "2.0"
