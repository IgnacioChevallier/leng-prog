-- {-# LANGUAGE RecordWildCards #-}

--import Data.Foldable (for_)
import Test.Hspec    (describe, hspec, it, shouldBe)
import qualified Data.Map as M

import Huffman

main :: IO ()
main = hspec $ do
  describe "huffman FrequencyMap" $ do
    it "Simple Text" $ do
      frequencyMap "PAPAYA" `shouldBe` M.fromList [('A', 3), ('P', 2), ('Y', 1)]
    it "Complex Text" $ do
      frequencyMap "zapallo con papa" `shouldBe` M.fromList [(' ', 2), ('a', 4), ('c', 1), ('l', 2), ('n', 1), ('o', 2), ('p', 3), ('z', 1)]
    it "Reversed Ascending Complex Text" $ do
      frequencyMapReversedAsc "zapallo con papa" `shouldBe` M.fromList [(1, 'c'), (1, 'n'), (1, 'z'),(2, ' '), (2, 'l'), (2, 'o'), (3, 'p'), (4, 'a')]

    it "Reversed Ascending Text" $ do
      frequencyMapReversedAsc "PAPAYA" `shouldBe` M.fromList [(1, 'Y'), (2, 'P'), (3, 'A')]
    it "Get List From Map" $ do
      getListFromMap (M.fromList [(1, 'Y'), (2, 'P'), (3, 'A')]) `shouldBe` [(1, Leaf 'Y'), (2, Leaf 'P'), (3, Leaf 'A')]
    it "From List To Head Simple" $ do
      fromListToHead [(1, Leaf 'Y'), (2, Leaf 'P')] `shouldBe` [(3, Leaf 'Y' :-: Leaf 'P')]
    it "From List To Head" $ do
      fromListToHead [(1, Leaf 'Y'), (2, Leaf 'P'), (3, Leaf 'A')] `shouldBe` [(6,Leaf 'A' :-: (Leaf 'Y' :-: Leaf 'P'))]

  describe "huffman Trie" $ do
    it "Empty input" $ do
      huffmanTrie "" `shouldBe` Empty
      
    it "Simple Text" $ do
     huffmanTrie "PAPAYA" `shouldBe` Leaf 'A' :-: (Leaf 'Y' :-: Leaf 'P')  

    it "Longer" $ do
     huffmanTrie "zapallo con papa" `shouldBe` 
        ((Leaf 'z' :-: Leaf ' ') :-: Leaf 'a')
           :-:
        (  (Leaf 'l' :-: Leaf 'o')
             :-: 
           ((Leaf 'c' :-: Leaf 'n') :-: Leaf 'p')) 
           
  describe "encode" $ do
    it "Empty input" $ do
      doEncode "" `shouldBe` []
      
    it "Simple Text" $ do
      doEncode "PAPAYA" `shouldBe` [T,T,F,T,T,F,T,F,F] 

    it "Longer" $ do
      doEncode "zapallo con papa" `shouldBe` [F,F,F,F,T,T,T,T,F,T,T,F,F,T,F,F,T,F,T,F,F,
                                              T,T,T,F,F,T,F,T,T,T,F,T,F,F,T,T,T,T,F,T,T,T,T,F,T]

  describe "decode"  $ do
    it "Empty input"  $ do
      doDecode "" [] `shouldBe` ""  
      
    it "Simple Text" $ do
      doDecode "PAPAYA" [T,T,F,T,T,F,T,F,F] `shouldBe` "PAPAYA"
      
    it "Longer" $ do
      doDecode "zapallo con papa"  
        [F,F,F,F,T,T,T,T,F,T,T,F,F,T,F,F,T,F,T,F,F,
         T,T,T,F,F,T,F,T,T,T,F,T,F,F,T,T,T,T,F,T,T,T,T,F,T] `shouldBe` "zapallo con papa"  
      
doEncode :: String -> [Bit]
doEncode s = encode s (huffmanTrie s)

doDecode :: String -> [Bit] -> String
doDecode s bs = decode bs (huffmanTrie s) 