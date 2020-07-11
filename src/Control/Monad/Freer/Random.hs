{-# LANGUAGE AllowAmbiguousTypes #-}

{-|
Module      : Control.Monad.Freer.Random
Description : Effect for generating random values
Copyright   : (c) Ben Weitzman 2018
License     : MIT
Maintainer  : ben@costarastrolgoy.com
Stability   : experimental
Portability : POSIX
-}
module Control.Monad.Freer.Random
  -- (Random
  -- ,random
  -- ,randomR
  -- -- ,runRandom
  -- ,knownRandom
  -- ,runRandomWithSeed
  -- ,silentRandom
  -- ,FindInList
  -- ,RandomBSGen
  -- ,randomByteString
  -- )
where

import Prelude hiding (find)
import Control.Monad.Freer
import Control.Monad.Freer.TH
import Control.Monad.Freer.Internal
import qualified System.Random as R
import Data.Typeable ((:~:)(..))
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as S

data Elem a as where
  Here :: Elem a (a ': as)
  There :: Elem a as -> Elem a (b ': as)

-- | Find a type inside of a type level list. Used to write code that generates multiple types of random values
-- that are polymorphic over the set of all random values that are generated.
--
-- @
-- getARandomNumber :: ('FindInList' 'Int' typs) => 'Eff' ('Random' typs ': r) 'Int'
-- getARandomNumber = random
--
-- getARandomBool :: ('FindInList' 'Bool' typs) => 'Eff' ('Random' typs ': r) 'Bool'
-- getARandomBool = random
--
-- getABoolAndInt :: ('FindInList' 'Bool' typs, 'FindInList' 'Int' typs) => 'Eff' ('Random' typs ': r) 'Int'
-- getABoolAndInt = do
--   rInt <- getARandomNumber
--   rBool <- getARandomBool
--   if rBool
--     then return rInt
--     else return $ rInt + 1
-- @
class FindInList a as where
  find :: Elem a as

instance {-# OVERLAPPING #-} FindInList a (a ': as) where
  find = Here

instance FindInList a as => FindInList a (b ': as) where
  find = There find

-- | The 'Random' effect generates a random value for you. It is parametrized by the set of values (@typs@)
-- that it generates. The 'Random' effect needs to come at the head of the effect list in order to make type
-- in order to make type inference of the set of types possible.
data Random typs a where
  Random :: (R.Random a) => Elem a typs -> Random typs a
  Uniform :: (R.Uniform a) => Elem a typs -> Random typs a
  RandomR :: (R.Random a) => Elem a typs -> (a, a) -> Random typs a

data RandomBSGen a where
    GenByteString :: Int -> RandomBSGen ByteString
makeEffect ''RandomBSGen

-- | Generate a single random value
random :: forall a typs r p. (R.Random a, FindInList a typs) => Eff (Random typs ': r) a
random = send $ Random (find @a @typs)

-- | Generate a single random value
randomUniform :: forall a typs r p. (R.Uniform a, FindInList a typs) => Eff (Random typs ': r) a
randomUniform = send $ Uniform (find @a @typs)

-- | Generate a single random value in a range
randomR :: forall a typs r p. (R.Random a, FindInList a typs) =>  (a, a) -> Eff (Random typs ': r) a
randomR range = send $ RandomR (find @a @typs) range

-- | Generate a ByteString of given size
-- randomByteString :: Int -> Eff '[RandomBSGen, effs] a
-- randomByteString size = send $ GenByteString size

-- | Use the 'IO' effect to handle generation of random values
-- runRandom :: forall t r a . Member IO r => Eff (Random t ': r) a -> Eff r a
-- runRandom = interpret $ \r -> case r of
--   Random _ -> send R.randomIO
--   Uniform _ -> send R.randomIO
--   RandomR _ range -> send $ R.randomRIO range

-- | Eliminate a 'Random' effect that doesn't generate any values
silentRandom :: Eff (Random '[] ': r) v -> Eff r v
silentRandom = interpret $ \r -> case r of

-- | Use a single given value as the "random" value. The given value is always used, even if it's
-- outside the range given to 'randomR'
knownRandom :: forall typ a r v . a -> Eff (Random (a ': typ) ': r) v -> Eff (Random typ ': r) v
knownRandom known = reinterpret $ \r -> case r of
  Random Here -> return known
  Random (There q) -> send $ Random q
  Uniform Here -> return known
  Uniform (There q) -> send $ Uniform q
  RandomR Here _ -> return known
  RandomR (There q) _ -> send $ Random q

-- | Use a seed + a PRNG to handle generation of random values.
runRandomWithSeed :: forall typ r v . Int -> Eff (Random typ ': r) v -> Eff r v
runRandomWithSeed seed = handleRelayS (R.mkStdGen seed) (\gen val -> return val) handler
  where
    handler :: R.StdGen -> Random typ a -> (R.StdGen -> a -> Eff r b) -> Eff r b
    handler gen (Random _) next =
      let r = R.random gen
      in next (snd r) (fst r)
    handler gen (Uniform _) next =
      let r = R.uniform gen
      in next (snd r) (fst r)
    handler gen (RandomR _ range) next =
      let r = R.randomR range gen
      in next (snd r) (fst r)

runRandomWithSeed2 :: forall r v . Int -> Eff (RandomBSGen ': r) v -> Eff r v
runRandomWithSeed2 seed = handleRelayS (R.mkStdGen seed) (\gen val -> return val) handler
  where
    handler :: R.StdGen -> RandomBSGen a -> (R.StdGen -> a -> Eff r b) -> Eff r b
    handler gen (GenByteString size) next =
        let r = R.genByteString size gen in
            next (snd r) (fst r)

-- | Pick a random value from a list
pickRandom :: (FindInList Int typ) => [a] -> Eff (Random typ ': r) (Maybe a)
pickRandom vals = do
  let max' = length vals
  idx <- randomR (0, max' - 1)
  return . listToMaybe $ drop idx vals

-- | Shuffle a set of values into a sequence
shuffle :: (FindInList Int typ) => Set a -> Eff (Random typ ': r) [a]
shuffle s
  | S.null s = return []
  | otherwise = do
      idx <- randomR (0, S.size s - 1)
      let val = S.elemAt idx s
      rest <- shuffle $ S.deleteAt idx s
      return $ val : rest
