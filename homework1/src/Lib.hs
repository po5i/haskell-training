module Lib
    ( toDigits
    , toDigitsRev
    , doubleEveryOther
    , sumDigits
    , validate
    , hanoi
    ) where

-- exercise 1
-- Example: toDigits 1234 == [1,2,3,4]
toDigits :: Integer -> [Integer]
toDigits x
    | x > 0 = toDigits(x `div` 10) ++ [x `mod` 10] -- recursion with whole number except the tail, add the tail to result
    | otherwise = []

-- Example: toDigitsRev 1234 == [4,3,2,1]
toDigitsRev :: Integer -> [Integer]
toDigitsRev x = reverse(toDigits x)

-- exercise 2
-- Example: doubleEveryOther [8,7,6,5] == [16,7,12,5]
doubleAux :: [Integer] -> [Integer]
doubleAux (x:y:xs) = x : 2 * y : doubleAux xs
doubleAux x = x

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther x = reverse(doubleAux (reverse x))

-- exercise 3
-- Example: sumDigits [16,7,12,5] = 1 + 6 + 7 + 1 + 2 + 5 = 22
splitDigits :: [Integer] -> [Integer]
splitDigits [] = []
splitDigits (x:xs)
    | x >= 10 = [x `div` 10] ++ [x `mod` 10] ++ splitDigits xs
    | otherwise = [x] ++ splitDigits xs -- Warning: use :

sumDigits :: [Integer] -> Integer
sumDigits x = sum(splitDigits x)

-- exercise 4
-- Example: validate 4012888888881881 = True
validate :: Integer -> Bool
validate x
    | x `mod` 10 /= 0 = False
    | otherwise = True


-- exercise 5 (Towers of Hanoi)
-- 1. move n − 1 discs from a to c using b as temporary storage = a c b
-- 2. move the top disc from a to b                             = (a,b)
-- 3. move n − 1 discs from c to b using a as temporary storage = c b a
type Peg = String
type Move = (Peg, Peg)
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 0 _ _ _ = []  -- zero disks pattern
hanoi n a b c = hanoi (n-1) a c b ++ [(a,b)] ++ hanoi (n-1) c b a
