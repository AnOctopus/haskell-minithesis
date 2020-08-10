import Relude

import TestCase

main :: IO ()
-- main = putStrLn "Test suite not yet implemented"


-- main = printResult $ check''' example3
main = do
    printResult $ check example
    printResult $ check example2
    printResult $ check example3
    printResult $ check example4
    pure ()
