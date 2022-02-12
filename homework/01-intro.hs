-- cis 194 week 01 homework
-- exercise 01
toDigits :: Integer -> [Integer]
toDigits n
  | n <= 0 = []
  | otherwise = toDigits (butLastDigit n) ++ [lastDigit n]

toDigitsRev :: Integer -> [Integer]
toDigitsRev n 
  | n <= 0 = []
  | otherwise = lastDigit n : toDigitsRev (butLastDigit n)

-- exercise 02
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther [] = []
doubleEveryOther (x:xs)
  | isEven (length (x:xs)) = x * 2 : doubleEveryOther xs
  | otherwise = x : doubleEveryOther xs

-- exercise 03
sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits (x:xs)
  | x > 9 = butLastDigit x + lastDigit x + sumDigits xs
  | otherwise = x + sumDigits xs

-- exercise 04
validate :: Integer -> Bool
validate n = sumDigits(doubleEveryOther (toDigits n)) `mod` 10 == 0

-- exercise 05
type Peg = String
type Move = (Peg, Peg)
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 0 _ _ _ = []
hanoi n peg peg' peg'' = hanoi (n - 1) peg peg'' peg' ++ [(peg, peg')] ++ hanoi (n - 1) peg'' peg' peg

-- exercise 06
-- optional, TODO

-- helpers
lastDigit :: Integer -> Integer
lastDigit n = n `mod` 10

butLastDigit :: Integer -> Integer
butLastDigit n = n `div` 10

isEven :: Int -> Bool
isEven n = n `mod` 2 == 0
