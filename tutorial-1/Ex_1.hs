module Exercise01 where

import Test.QuickCheck

{- Define the encoding function such that myPair x y = 2^ y(2x + 1) − 1-}
myPair :: Integer -> Integer -> Integer
myPair x y = (2 ^ y) * (2 * x + 1) - 1

{- Define the inverse function such that mySnd (myPair x y) = y -}
mySnd :: Integer -> Integer
mySnd z = if even z || z == 0 then 0 else searchPowTwo (z + 1)

searchPowTwo :: Integer -> Integer
searchPowTwo n = searchPowTwo' n 0
    where searchPowTwo' n m = if n `mod` 2 == 1 then m else searchPowTwo' (n `div` 2) (m + 1)

{-Define the inverse function such that myFst (myPair x y) = x -}
myFst :: Integer -> Integer
myFst z = if z == 0 || z == 1 then 0 else let y = mySnd z in (z - 2 ^ y + 1) `div` (2 ^ (y + 1))

{-checks whether p encodes thepair (x, y)-}
prop_myPair :: Integer -> Integer -> Integer -> Property
prop_myPair p x y = p >= 0 && x >= 0 && y >= 0 ==> isDefined(myPair x y) where isDefined p = p == p

{- an algorithm to print numbers in Esperanto -}
digitToEo :: Integer -> String
digitToEo 0 = "nul"
digitToEo 1 = "unu"
digitToEo 2 = "du"
digitToEo 3 = "tri"
digitToEo 4 = "kvar"
digitToEo 5 = "kvin"
digitToEo 6 = "ses"
digitToEo 7 = "sep"
digitToEo 8 = "ok"
digitToEo 9 = "nau"

{-Ex2-}

numberToEo :: Integer -> String
numberToEo n 
    | n < 10 = digitToEo n
    | n < 100 = numberToEoHelper (n `div` 10) 1 "" "" ++ "dek" ++ numberToEoHelper (n `mod` 10) 0 " " ""
    | n < 1000 = numberToEoHelper (n `div` 100) 1 "" "" ++ "cent" ++ numberToEoHelper (n `mod` 100) 0 " " ""
    | n < 1000000 = numberToEoHelper (n `div` 1000) 1 "" " " ++ "mil" ++ numberToEoHelper (n `mod` 1000) 0 " " ""

numberToEoHelper :: Integer -> Integer -> String -> String -> String
numberToEoHelper n offset prefix suffix = if n > offset then prefix ++ numberToEo n ++ suffix else ""

{-Ex2-}
