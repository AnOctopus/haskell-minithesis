{-# LANGUAGE StrictData #-}
module GenTree where

import Relude

import qualified System.Random as R

import Internal.Util
import Gen (Cache)

data ChoiceTree = Node {choice :: Word64, children :: [ChoiceTree], size :: Natural}
    deriving stock (Eq, Show, Generic)

data Choices = Choices {
    unTree :: ChoiceTree,
    unIndex :: Index,
    unMaxVal :: Natural,
    unGen :: R.StdGen,
    unCache :: Cache
                         }
    deriving stock (Show, Generic)

newtype ChoiceState a = ChoiceState {runChoiceState :: StateT Choices Maybe a}
    deriving newtype (Functor, Applicative, Monad, MonadState Choices, MonadFail)

newtype Gen a = Gen {runGen :: ChoiceState a}
    deriving (Functor, Applicative, Monad)


{-
This module should eventually be an alternative implementation of the choice structure, using a data type based on a rose tree.

The design of minithesis is interesting and has some nice advantages. However, the choice sequence is opaque, and shrinking behaves blindly wrt the structure implied by the choices.

An idea I had for improving this is to instead represent the choices in a tree, with related choices kept together and dependencies captured by a child relationship. I thought through how the creation and navigation of the tree would work, and the design I came up with involves creating some choice-making primitives that can manage the position in the choice tree, rather than having that entirely handled uniformly by incrementing an index.

Instead of an index, the position in the tree can be tracked using a stack of child indices. The core choice creation function will create a new child from the current position and update our position to point to it. There will additionally be a function for popping the stack, to allow for more fine grained control to provide good structure for shrinking.

Shrinking would be split into two groups. Generic shrinks would be applicable to any generator's choices, and would not take advantage of any special structure, but would provide a solid baseline for all generators.
-}
