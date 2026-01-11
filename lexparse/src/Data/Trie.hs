module Data.Trie where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map


-- | Trie (prefix tree) implementation for storing sequences of elements.
--
-- Each 'Node' contains:
--   * a boolean flag indicating whether a word ends here ('True' for terminal node)
--   * a map from elements to child subtrees
--
-- 'Empty' represents an empty trie.
data TrieTree a = Empty | Node !Bool !(Map a (TrieTree a)) deriving (Eq, Show)


-- | Add a word (list of elements) to a trie tree.
--
-- If the word already exists, the trie remains unchanged.
--
-- Example:
-- > add "cat" Empty
-- > add "car" (add "cat" Empty)
add :: Ord a => [a] -> TrieTree a -> TrieTree a
add [] Empty = Node True Map.empty
add [] (Node _ children) = Node True children
add (x:xs) Empty = Node False (Map.singleton x (add xs Empty))
add (x:xs) (Node term children) = Node term $ Map.alter f x children
  where
    f Nothing = Just (add xs Empty)
    f (Just sub) = Just (add xs sub)


-- | Build a trie tree from a list of words.
--
-- Example:
-- > build ["cat", "car", "dog"]
-- Creates a trie containing "cat", "car", and "dog".
build :: Ord a => [[a]] -> TrieTree a
build = foldr add Empty


-- | Get all prefix lengths of the input sequence that are valid words in the trie.
--
-- Returns a list of lengths where each prefix ending at that length exists as a word.
--
-- Example:
-- > prefixLengths trie "cart"
-- might return [2,3] if "ca" and "car" are words in the trie.
prefixLengths :: Ord a => TrieTree a -> [a] -> [Int]
prefixLengths t xs = go xs t 0
  where
    go _ Empty _ = []
    go [] (Node term _) idx = [idx | term]
    go (c:cs) (Node term subs) idx = [idx | term] ++ maybe [] (\sub -> go cs sub (idx + 1)) (Map.lookup c subs)


-- | Get the length of the longest prefix of the input sequence that is a word in the trie.
--
-- Returns 0 if no prefix matches.
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


-- | Split the input sequence into the longest matching prefix and the remainder.
--
-- Example:
-- > eatLongest trie "cartoon"
-- might return ("car", "toon") if "car" is the longest word prefix.
eatLongest :: Ord a => TrieTree a -> [a] -> ([a], [a])
eatLongest t x = splitAt (longestPrefixLength t x) x


-- | Check if a word exists in the trie.
--
-- Example:
-- > contains trie "cat" == True
-- > contains trie "cap" == False
contains :: Ord a => TrieTree a -> [a] -> Bool
contains Empty _ = False
contains (Node term _) [] = term
contains (Node _ children) (x:xs) = maybe False (`contains` xs) (Map.lookup x children)


-- | Check if the trie contains a prefix of a word.
--
-- Returns 'True' if the trie has any word starting with the given sequence.
--
-- Example:
-- > hasPrefix trie "ca" == True
-- > hasPrefix trie "xy" == False
hasPrefix :: Ord a => TrieTree a -> [a] -> Bool
hasPrefix Empty _ = False
hasPrefix (Node _ _) [] = True
hasPrefix (Node _ children) (x:xs) = maybe False (`hasPrefix` xs) (Map.lookup x children)