{-# LANGUAGE OverloadedStrings #-}
module ByteState where


{-
What do I need to implement this (MVP)?

A type representing the underlying bytestream. Can I just use ByteString and Random's genByteString?
    This will ultimately need to hang on to the values it produces, but that seems taken care of by keeping it as a state in the first place
A generator function `ByteString -> Maybe a` that produces values using the random data
    Rely on `read`, because I don't think we can implement this ourselves
A shrink function `ByteString -> ByteString` that tries to reduce the bytes to something simpler that still results in values that cause the test to fail

How do I rerun the tests with the new bytestring?
    This is done by keeping the state and calling runState with new values
How do I run the tests, then roll back if the changes don't work?
    As above
How do I maintain the bytestring for each generator as separate?
    Fake it with a list of them, for now
-}

import Prelude hiding (State, get, put, evalState, modify, splitAt)
-- import GHC.List (lookup)
-- import Control.Monad.Random.Lazy
-- import qualified Data.ByteString.Lazy.Char8 as C
-- import Data.ByteString.Lazy.Char8 (ByteString, unpack)
import qualified Data.List as Unsafe
-- import qualified Text.Read as R

-- import qualified Relude.Unsafe as Unsafe
import Data.List.NonEmpty (splitAt)

import Control.Monad.Freer
import Control.Monad.Freer.TH
import Control.Monad.Freer.State
import Control.Monad.Freer.Random


{-
genValue idx type = do
    bytes <- getOrCreate idx
    return type `parseFrom` bytes

tryShrinkValue idx type = do
    bytes <- get idx
    return tryRandomShrink bytes

storeSuccessfulShrink idx bytes = do
    put idx bytes
-}


-- runByteState :: Eff '[ByteState] a -> ByteString
-- runByteState = run . interpret (
--     \case
--         GetOrCreate idx -> getOrCreateF idx
--     )



-- getOrCreateF idx = undefined

-- data FileSystem r where
--   ReadFile :: FilePath -> FileSystem String
--   WriteFile :: FilePath -> String -> FileSystem ()

-- runInMemoryFileSystem :: [(FilePath, String)] -> Eff (FileSystem ': effs) ~> Eff effs
-- runInMemoryFileSystem initVfs = evalState initVfs . fsToState where
--   fsToState :: Eff (FileSystem ': effs) ~> Eff (State [(FilePath, String)] ': effs)
--   fsToState = reinterpret $ \case
--     ReadFile path -> get >>= \vfs -> case lookup path vfs of
--       Just contents -> pure contents
--       Nothing -> error (toText $ "readFile: no such file " ++ path)
--     WriteFile path contents -> modify $ \vfs ->
--       (path, contents) : deleteBy ((==) `on` fst) (path, contents) vfs


-- byteStateToState :: Eff (ByteState ': effs) ~> Eff (State [ByteString] ': effs)
-- byteStateToState = reinterpret $ \case
--     GetOrCreate idx size -> get >>= \st -> case st !!? idx of
--         Just contents -> pure contents
--         Nothing -> do
--             let newVal = C.singleton 'a'
--             modify (<> [newVal])
--             return newVal

-- data Rand r where
--     FromSeed :: Rand Char

-- runRand ::
--     Eff '[Rand, effs] ~>
--     Eff '[Random '[Char], State Int, effs]
-- runRand = reinterpret2 $ \case
--     FromSeed -> do
--         s <- get :: Eff '[Random '[Char], State Int, effs] Int
--         randomR ('a', 'c')

-- runRandomFromState ::
--     Eff '[Random typs, State Int, eff] ~>
--     Eff effs
-- runRandomFromState = undefined

type Index = Int
data ByteState r where
    GetOrCreate :: Index -> Int -> ByteState ByteString
    Update :: Index -> ByteString -> ByteState ()

runByteState :: Eff '[ByteState, effs] ~> Eff '[State [ByteString], RandomBSGen, effs]
runByteState = reinterpret2 $ \case
    GetOrCreate idx size -> do
        st <- get
        let val = case st !!? idx of
                Just contents -> pure contents
                Nothing -> do
                    newVal <- genByteString size
                    modify (<> [newVal])
                    pure newVal
        val
    Update idx val -> do
        st <- get
        put $ replace2 st idx val
    -- Shrink idx -> do
    --     st <- get
    --     val <- getOrCreate idx 0
    --     let newVal = shrinkBytes val
    --     put $ replace2 st idx newVal
    --     pure newVal

data Gen typ r where
    GenVal :: ByteString -> Gen typ r

-- genVal :: Eff '[Gen typ, effs]

shrinkBytes :: ByteString -> ByteString
shrinkBytes = id

-- | Replace index i in list l with value v
replace :: NonEmpty a -> Index -> a -> NonEmpty a
replace l i v =
    let (h, t) = splitAt (i - 1) l
        r = Unsafe.tail h
    in
        head l :| r <> [v] <> Unsafe.tail t

replace2 :: [a] -> Index -> a -> [a]
replace2 l i v =
    let (h, t) = Unsafe.splitAt (i - 1) l
        r = Unsafe.tail h
    in
        Unsafe.head l : r <> [v] <> Unsafe.tail t

(!!??) :: NonEmpty a -> Index -> Maybe a
l !!?? i = toList l !!? i

-- newtype Read a => Gen a = Gen {unGen :: Bytes -> Maybe a}

-- mkInt :: Bytes -> Maybe Int
-- mkInt = R.readMaybe . C.unpack

-- intGen :: Gen Int
-- intGen = Gen mkInt


-- type Bytes = RandT StdGen C.ByteString

-- shrinkIntGen :: Bytes -> Bytes
-- shrinkIntGen = id

-- first :: (a -> b) -> (a, c) -> (b, c)
-- first f (a, c) = (f a, c)

-- genByteString :: RandT StdGen Identity C.ByteString
-- genByteString = C.pack <$> getRandoms

-- drawBytes ::  Int -> RandT StdGen (State [C.ByteString]) ()
-- -- drawBytes :: Int -> State [RandT StdGen Identity C.ByteString] ()
-- -- drawBytes :: Int -> StateT [C.ByteString] (Rand StdGen) ()
-- --  If there is data there, return that, otherwise generate new data and append it, then advances the index by 1
-- drawBytes idx = do
--     -- bs :: ByteList
--     bs <- get
--     let
--         bi = bs !!? idx
--         b = case bi of
--                  Nothing -> do
--                      np <- lift genByteString
--                      put (bs <> [np]) -- :: RandT StdGen (State [C.ByteString]) ()

--                      return np
--                  -- Just bytes -> return $ return bytes
--                  Just bytes -> undefined

--     pure ()

makeEffect ''ByteState
-- makeEffect ''Gen
