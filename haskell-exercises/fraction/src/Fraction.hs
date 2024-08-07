module Fraction (Fraction, add, sub, mul, divide, hcf) where


type Fraction = (Int, Int)


-- Implement the `add` Function


add :: Fraction -> Fraction -> Fraction
add (n1, d1) (n2, d2)
 | d1 == d2 = (n1 + n2, d1)
 | otherwise = (n1 * d2 + n2 * d1, d1 * d2)


-- Implement the `sub` Function


sub :: Fraction -> Fraction -> Fraction
sub (n1, d1) (n2, d2)
 | d1 == d2 = (n1 - n2, d1)
 | otherwise = (n1 * d2 - n2 * d1, d1 * d2)


-- Implement the `mul` Function


mul :: Fraction -> Fraction -> Fraction
mul (n1, d1) (n2, d2) = simplify (n1 * n2, d1 * d2)


-- Implement the `divide` Function


divide :: Fraction -> Fraction -> Fraction
divide (n1, d1) (n2, d2) = (n1 * d2, d1 * n2)


-- Implement the `hcf` Function


hcf :: Int -> Int -> Int
hcf n d
 | n < d = searchDivisor n d n
 | otherwise = searchDivisor d n d


searchDivisor :: Int -> Int -> Int -> Int
searchDivisor n d h
 | d `mod` h == 0 && n `mod` h == 0 = h
 | otherwise = searchDivisor n d (h - 1)


simplify :: Fraction -> Fraction
simplify (n, d) = (n `div` h, d `div` h)
 where h = hcf n d
