module Main where

import Lib

main :: IO ()
main = do
  print "Input a credit card number"
  ccNumber <- getLine
  let ccNumberInt = read ccNumber :: Integer
  -- print "Exercise 1"
  -- let numberList = toDigits ccNumberInt
  -- let numberListRev = toDigitsRev ccNumberInt
  -- print numberList
  -- print numberListRev
  -- print "Exercise 2"
  -- let doubled = doubleEveryOther(toDigits ccNumberInt)
  -- print doubled
  -- print "Exercise 3"
  -- let sumAll = sumDigits (doubleEveryOther(toDigits ccNumberInt))
  -- print sumAll
  -- print "Exercise 4"
  print (validate $ sumDigits $ doubleEveryOther $ toDigits ccNumberInt)
  print "-------------------"
  print "The Towers of Hanoi"
  print "Number of disks:"
  disks <- getLine
  let disksInt = read disks :: Integer
  print "Name of first peg (source)"
  a <- getLine
  print "Name of second peg (target)"
  b <- getLine
  print "Name of third peg (temp)"
  c <- getLine
  print (hanoi disksInt a b c)
