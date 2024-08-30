module Huffman  (huffmanTrie, encode, decode, Trie(..), Bit(..), frequencyMap, frequencyMapReversedAsc, getListFromMap, fromListToHead) where

import qualified Data.Map as M

data Bit = F | T deriving (Eq, Show)
type Bits = [Bit]  

data Trie a = Empty
            | Leaf a
            | Trie a :-: Trie a deriving (Eq, Show, Ord)

huffmanTrie::String -> Trie Char
huffmanTrie "" = Empty
huffmanTrie input =
    getTrie (fromListToHead (getListFromMap (frequencyMapReversedAsc input)))

-- FrequencyMap to list of [(Int, Leaf Char)]
getListFromMap:: M.Map Int Char -> [(Int, Trie Char)]
getListFromMap mapa = M.toList (M.map (\x -> Leaf x) mapa)

fromListToHead :: [(Int, Trie Char)] -> [(Int, Trie Char)]
fromListToHead [] = []
fromListToHead [x] = [x]
fromListToHead (x:y:xs) = fromListToHead (((n + m), (snd x :-: snd y)) : xs)
    where n = fst x
          m = fst y

insertSorted :: (Int, Trie Char) -> [(Int, Trie Char)] -> [(Int, Trie Char)]
insertSorted new [] = [new]
insertSorted new@(newFreq, _) (x:xs)
  | newFreq <= fst x = new : x : xs
  | otherwise = x : insertSorted new xs


getTrie:: [(Int, Trie Char)] -> Trie Char
getTrie [] = Empty
getTrie [x] = snd x
getTrie (x:xs) = getTrie ((fst x, (snd x :-: snd (head xs))) : tail xs)

encode :: String -> Trie Char -> Bits
encode input code = error "Implement it"
  
decode::Bits -> Trie Char -> String
decode bits trie = error "Implement it"

-- Imports from frequencies, solo necesito el Map Int Char
frequencyMap :: [Char] -> M.Map Char Int
frequencyMap = foldr (\c -> M.insertWith (+) c 1) M.empty

frequencyMapReversedAsc :: [Char] -> M.Map Int Char
frequencyMapReversedAsc = ascendingOrder. rotateMap . frequencyMap

rotateMap :: M.Map a Int -> M.Map Int a
rotateMap = M.fromList . map (\(a, b) -> (b, a)) . M.toList

ascendingOrder :: M.Map Int a -> M.Map Int a
ascendingOrder = M.fromList . reverse . M.toAscList