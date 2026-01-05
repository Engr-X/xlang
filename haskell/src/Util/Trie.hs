module Util.Trie where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map


data TrieTree a = Empty | Node !Bool !(Map a (TrieTree a)) deriving (Eq, Show)

-- | add a word to trie tree
add :: Ord a => [a] -> TrieTree a -> TrieTree a
add [] Empty = Node True Map.empty
add [] (Node _ children) = Node True children
add (x:xs) Empty = Node False (Map.singleton x (add xs Empty))
add (x:xs) (Node term children) = Node term $ Map.alter f x children
  where
    f Nothing = Just (add xs Empty)
    f (Just sub) = Just (add xs sub)

-- | build trie tree
build :: Ord a => [[a]] -> TrieTree a
build = foldr add Empty

-- | get all prefix lengths
prefixLengths :: Ord a => TrieTree a -> [a] -> [Int]
prefixLengths t xs = go xs t 0
  where
    go _ Empty _ = []
    go [] (Node term _) idx = [idx | term]
    go (c:cs) (Node term subs) idx = [idx | term] ++ maybe [] (\sub -> go cs sub (idx + 1)) (Map.lookup c subs)

-- | get longest prefix length
longestPrefixLength :: Ord a => TrieTree a -> [a] -> Int
longestPrefixLength t xs = go xs t 0 0
  where
    go _ Empty _ best = best
    go [] (Node term _) acc best = if term then acc else best
    go (c:cs) (Node term subs) acc best =
        let best' = if term then acc else best
        in case Map.lookup c subs of
            Nothing  -> best'
            Just sub -> go cs sub (acc + 1) best'

-- | get longest prefix
eatLongest :: Ord a => TrieTree a -> [a] -> ([a], [a])
eatLongest t x = splitAt (longestPrefixLength t x) x

-- | check if trie tree contains a word
contains :: Ord a => TrieTree a -> [a] -> Bool
contains Empty _ = False
contains (Node term _) [] = term
contains (Node _ children) (x:xs) = maybe False (`contains` xs) (Map.lookup x children)

-- | check if trie tree has a prefix
hasPrefix :: Ord a => TrieTree a -> [a] -> Bool
hasPrefix Empty _ = False
hasPrefix (Node _ _) [] = True
hasPrefix (Node _ children) (x:xs) = maybe False (`hasPrefix` xs) (Map.lookup x children)