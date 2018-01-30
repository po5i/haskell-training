module Lib
    ( toDigits
    , toDigitsRev
    , doubleEveryOther
    , sumDigits
    , validate
    , hanoi
    ) where
-------------
-- exercise 1
-- toDigits: converts an integer into a list of its digits
-- Example: toDigits 1234 == [1,2,3,4]
toDigits :: Integer -> [Integer]
toDigits x
    -- recursion with whole number except the tail, add the tail to result
    | x > 0 = toDigits(x `div` 10) ++ [x `mod` 10]
    | otherwise = []

-- toDigitsRev: converts an integer into a inverted list of its digits
-- Example: toDigitsRev 1234 == [4,3,2,1]
toDigitsRev :: Integer -> [Integer]
toDigitsRev x = reverse(toDigits x)

-------------
-- exercise 2
-- doubleEveryOther:  double every other number beginning from the right
-- using the auxiliary function doubleOtherForward (which does that from 
-- left to right)
-- Example: doubleEveryOther [8,7,6,5] == [16,7,12,5]
doubleOtherForward :: [Integer] -> [Integer]
doubleOtherForward (x:y:xs) = x : 2 * y : doubleOtherForward xs
doubleOtherForward x = x

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther x = reverse(doubleOtherForward (reverse x)) -- reverse,reverse?

-------------
-- exercise 3
-- sumDigits: sum up every digit that are part of the list, using the 
-- auxiliary function splitDigit which sum up every digit for elements >= 10
-- Example: sumDigits [16,7,12,5] = 1 + 6 + 7 + 1 + 2 + 5 = 22
splitDigits :: [Integer] -> [Integer]
splitDigits [] = []
splitDigits (x:xs)
    | x >= 10 = [x `div` 10] ++ [x `mod` 10] ++ splitDigits xs
    | otherwise = [x] ++ splitDigits xs -- Warning: use :

sumDigits :: [Integer] -> Integer
sumDigits x = sum(splitDigits x)

-------------
-- exercise 4
-- validate: validate if a creditar card checksum by calculating the remainder
-- when the sum is divided by 10. If result = 0, then the number is valid
-- Example: validate 4012888888881881 = True
validate :: Integer -> Bool
validate x
    | x `mod` 10 /= 0 = False
    | otherwise = True

-------------------------------
-- exercise 5 (Towers of Hanoi)
-- hanoi: receives 4 arguments: number of disks and the name for 3 pegs.
-- Outputs the movements for the towers of Hanoi puzzle.
-- 1. move n − 1 discs from a to c using b as temporary storage = a c b
-- 2. move the top disc from a to b                             = (a,b)
-- 3. move n − 1 discs from c to b using a as temporary storage = c b a
type Peg = String
type Move = (Peg, Peg)
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 0 _ _ _ = []  -- zero disks pattern
hanoi n a b c = hanoi (n-1) a c b ++ [(a,b)] ++ hanoi (n-1) c b a
