module TestCase where

import Relude hiding (pred)
import qualified Relude.Unsafe as Unsafe

import qualified Data.Vector.Unboxed as V
import qualified System.Random as R

import Gen
import Shrink
import Internal.Data.Tree


(===) :: Eq a => a -> a -> Property a
a === b = do
    let r = if a == b
            then Valid
            else Failure a b
    pure r

(/==) :: Eq a => a -> a -> Property a
a /== b =  do --[i|a: #{a}, b: #{b}|] $ do
    let r = if a /= b
            then Valid
            else Failure a b
    pure r

type Property a = Gen (PropertyResult a)
type Test a = Property a -> PropertyResult a


assert :: Show a => Bool -> a -> Property Bool
assert b msg = Gen . pure $ if b then Valid else AssertFailure $ show msg

pred :: StateT Choices Maybe (PropertyResult a) -> Choices -> ShrinkState Bool
pred test choice = res2
    where
        choiceSeq = V.toList $ unBytes choice
        ~res2 = do
            trie <- get
            case lookup choiceSeq trie of
                Nothing -> res'
                Just cachedResult -> --trace "cache hit"
                    pure $ interesting cachedResult
        ~res' = do
            case runStateT test choice of
                Nothing -> do
                    trie <- get
                    put $ insert choiceSeq Overrun trie
                    pure False
                Just (r2, c2) -> do
                    trie <- get
                    put (insert choiceSeq (asTestResult r2) trie)
                    let r3 = case r2 of
                            Failure _ _ -> case c2 of
                                Choices {unIndex=i, unMaxVal=v}
                                    | fromIntegral i > v -> False
                                    | unBytes c2 > unBytes choice -> False
                                    | otherwise -> True
                            AssertFailure _ -> True
                            _ -> False
                    pure r3


runOne :: Gen a -> Int -> Maybe (a, Choices)
runOne cs n = do
    let
        g = R.mkStdGen n
        test = runChoiceState $ runGen cs
        mRC = runStateT test (Choices V.empty 0 (8 * 1024) g)
    mRC


checkOne :: Property a -> Int -> (PropertyResult a)
checkOne cs n = do
    let
        test = runChoiceState $ runGen cs
        mRC = runOne cs n
        r = fromMaybeProp (fst <$> mRC)
    if notable r then do
        let
            c = Unsafe.fromJust (snd <$> mRC)
            next = shrinkToFixpoint c $ pred test
            mRC' = runStateT test next
            fmp = fromMaybeProp (fst <$> mRC')
        -- trace ("next " <> show next <> " mRC' " <> show mRC' <> " fmp " <> show fmp) $ fmp
        fmp
        else r

notable :: PropertyResult a -> Bool
notable = \case
    Failure _ _ -> True
    AssertFailure _ -> True
    _ -> False

checkSome :: Property a -> [Int] -> [PropertyResult a]
checkSome cs = fmap (cs `checkOne`)

checkOneR :: Property a -> IO (PropertyResult a, Int)
checkOneR cs = do
    g <- R.newStdGen
    let n = fromIntegral $ fst $ R.genWord64 g
        res = checkOne cs n
    pure (res, n)

check :: Property a -> IO (PropertyResult a)
check cs = do
    -- tests <- replicateM 100 $ checkOneR cs
    seeds <- replicateM 100 $ R.randomIO
    let tests = zip (checkSome cs seeds) seeds
    let failed = (interestingProp . fst) `filter` tests
        firstFailed = if not (null failed) then Unsafe.head failed else Unsafe.head tests
    print (snd firstFailed)
    pure (fst firstFailed)

fromMaybeProp :: Maybe (PropertyResult a) -> PropertyResult a
fromMaybeProp = fromMaybe Invalid

printResult :: Show a => IO (PropertyResult a) -> IO ()
printResult res0 = do
    res <- res0
    case res of
        Valid -> putStrLn "Test passed"
        Failure a b -> putStrLn $ "LHS: " <> show a <> " RHS: " <> show b
        AssertFailure s -> putStrLn $ "Assertion invalidated by " <> show s
        Invalid -> putStrLn "Invalid"
    pure ()

