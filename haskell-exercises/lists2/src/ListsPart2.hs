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

queens::Int -> [Solution]
queens n = solve n
  where
    solve 0 = [[]]  -- Base case: one solution for a 0x0 board (empty board)
    solve k = [q:qs | qs <- solve (k-1), q <- [1..n], safe q qs 1]

    -- Check if placing a queen in column q is safe from attacks
    safe :: Int -> [Int] -> Int -> Bool
    safe _ [] _ = True
    safe q (q':qs) dist =
      q /= q' && q /= q' + dist && q /= q' - dist && safe q qs (dist + 1)
