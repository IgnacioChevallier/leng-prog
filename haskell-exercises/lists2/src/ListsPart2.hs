module ListsPart2 (Bit(..), bitAt, charToBits, bits, queens) where

import Data.Char(ord)  
import Data.Bits(testBit)
  
data Bit = F | T  deriving (Eq, Show, Enum, Read)
type Bits = [Bit]

bitAt :: Int -> Char -> Bit
bitAt n c = if testBit (ord c) (7-n) then T else F 

charToBits :: Char -> Bits
charToBits c = [bitAt n c | n <- [0..7]]

bits::String -> Bits
bits = foldl (\acc c -> acc ++ charToBits c) []

type Solution = [Int]

queens :: Int -> [Solution]
queens size = solve 1 [[]]
  where
    solve i solutions
      | i > size = solutions
      | otherwise = solve (i + 1) [j : solution | j <- [1 .. size], solution <- solutions, isSafe j 1 solution]
    isSafe _ _ [] = True
    isSafe j offset (c : cs) = j /= c && j /= c - offset && j /= c + offset && isSafe j (offset + 1) cs