module Trie  (Trie(..), left, right, find, decode, toList) where

import Bit

data Trie a = Nil | Leaf a | (Trie a) :-: (Trie a)
            deriving (Eq, Show)

left::Trie a -> Trie a
left trie = case trie of
  Nil -> Nil
  (Leaf _) -> Nil
  (l :-: _) -> l

right::Trie a -> Trie a
right trie = case trie of
    Nil -> Nil
    (Leaf _) -> Nil
    (_ :-: r) -> r

find :: Bits -> Trie a -> a
find [] (Leaf a) = a
find [] _ = error "Not found"
find (x:xs) (l :-: r) = if x == F then find xs l else find xs r
find _ _ = error "Invalid pattern"

decode :: Bits -> Trie Char -> String
decode [] _ = ""
decode bits trie = let (prefix, rest) = splitPrefix bits trie
                   in find prefix trie : decode rest trie

-- Helper function to split the bits into a prefix that leads to a leaf and the rest
splitPrefix :: Bits -> Trie a -> (Bits, Bits)
splitPrefix [] _ = ([], [])
splitPrefix bits@(x:xs) node = case node of
  Leaf _ -> ([], bits)
  l :-: r -> if x == F
             then let (prefix, rest) = splitPrefix xs l in (x:prefix, rest)
             else let (prefix, rest) = splitPrefix xs r in (x:prefix, rest)
  _ -> ([], bits)

toList::Trie a -> [(a, Bits)]
toList Nil = []
toList (Leaf a) = [(a, [])]
toList (l :-: r) = map (addBit F) (toList l) ++ map (addBit T) (toList r)
  where addBit b (a, bits) = (a, b:bits)
