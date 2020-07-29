{-# LANGUAGE StrictData #-}
module Tree where

import Relude

import qualified Data.Map.Strict as Map
import qualified Data.Vector.Unboxed as V

data MapTrie k v = MapTrie {mapNode :: Maybe v, mapChildren :: Map.Map k (MapTrie k v)}
    deriving (Show, Eq, Ord, Functor, Foldable, Traversable, Generic)

insert :: forall k v. Ord k => [k] -> v -> MapTrie k v -> MapTrie k v
insert (k:ks) v tree = next
    where
        next = case Map.lookup k (mapChildren tree) of
            -- k is not in the children, so create a new trie to insert that as a child of the current node
            Nothing -> t'
                where
                    newTrie = MapTrie {mapNode = Nothing, mapChildren = Map.empty}
                    m' = Map.insert k newTrie (mapChildren tree)
                    t' = insert (k:ks) v $ MapTrie {mapNode = Nothing, mapChildren = m'}
            -- k is in the children map, so recurse into it
            Just trie -> newTrie
               where
                   trie' = insert ks v trie
                   newMap = Map.insert k trie' (mapChildren tree)
                   newTrie = tree {mapChildren = newMap}
insert [] v tree = newTree
    where
        newTree = tree {mapNode = Just v}

lookup :: forall k v. Ord k => [k] -> MapTrie k v -> Maybe v
lookup (k:ks) tree = case Map.lookup k (mapChildren tree) of
    Nothing -> Nothing
    Just t' -> lookup ks t'
lookup [] tree = mapNode tree <|> Nothing

empty :: MapTrie k v
empty = MapTrie {mapNode = Nothing, mapChildren = Map.empty}
