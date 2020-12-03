module Internal.Data.Tree where

import Relude

import qualified Data.IntMap.Strict as Map

data MapTrie v = MapTrie {mapNode :: Maybe v, mapChildren :: Map.IntMap (MapTrie v)}
    deriving (Show, Eq, Ord, Functor, Foldable, Traversable, Generic)

insert :: forall v. [Word64] -> v -> MapTrie v -> MapTrie v
insert (k:ks) v tree = next
    where
        next = case Map.lookup (fromIntegral k) (mapChildren tree) of
            -- k is not in the children, so create a new trie to insert that as a child of the current node
            Nothing -> t'
                where
                    newTrie = MapTrie {mapNode = Nothing, mapChildren = Map.empty}
                    m' = Map.insert (fromIntegral k) newTrie (mapChildren tree)
                    t' = insert (k:ks) v $ MapTrie {mapNode = Nothing, mapChildren = m'}
            -- k is in the children map, so recurse into it
            Just trie -> newTrie
               where
                   trie' = insert ks v trie
                   newMap = Map.insert (fromIntegral k) trie' (mapChildren tree)
                   newTrie = tree {mapChildren = newMap}
insert [] v tree = newTree
    where
        newTree = tree {mapNode = Just v}

lookup :: forall v. [Word64] -> MapTrie v -> Maybe v
lookup (k:ks) tree = case Map.lookup (fromIntegral k) (mapChildren tree) of
    Nothing -> Nothing
    Just t' -> lookup ks t'
lookup [] tree = mapNode tree <|> Nothing

empty :: MapTrie v
empty = MapTrie {mapNode = Nothing, mapChildren = Map.empty}
