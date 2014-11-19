{-# OPTIONS_GHC -Wall #-}

toDigits :: Integer -> [Integer]
toDigits n
    | n <= 0    = []
    | otherwise = toDigits (n `div` 10) ++ [n `mod` 10]

toDigitsRev :: Integer -> [Integer]
toDigitsRev n
    | n <= 0    = []
    | otherwise = n `mod` 10 : toDigitsRev (n `div` 10)

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther [] = []
doubleEveryOther (x:xs)
    | even (length xs) = x : doubleEveryOther xs
    | otherwise        = 2 * x : doubleEveryOther xs

sumDigits :: [Integer] -> Integer
sumDigits []     = 0
sumDigits (x:xs) = sum (toDigits x) + sumDigits(xs)

validate :: Integer -> Bool
validate x = sumDigits (doubleEveryOther $ toDigits x) `mod` 10 == 0

type Peg = String
type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi n a b c
    | n <= 0    = []
    | otherwise = hanoi (n - 1) a c b ++ [(a, b)] ++ hanoi (n-1) c b a

hanoi3Moves :: Integer -> Int
hanoi3Moves n = length $ hanoi n "a" "b" "c"

-- TODO, NOT correct
hanoi4 :: Integer -> Peg -> Peg -> Peg -> Peg -> [Move]
hanoi4 n a b c d
    | n <= 0 = []
    | n == 1 = [(a, b)]
    | otherwise = hanoi4 (n - 2) a c b d ++ [(a, d), (a, b), (d, b)] ++ hanoi4 (n - 2) c b a d

hanoi4Moves :: Integer -> Int
hanoi4Moves n = length $ hanoi4 n "a" "b" "c" "d"
