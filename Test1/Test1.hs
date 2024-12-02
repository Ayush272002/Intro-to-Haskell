-- see https://wiki.haskell.org/Safe_Haskell
{-# LANGUAGE Safe #-}
-- see https://wiki.haskell.org/Safe_Haskell
{-# LANGUAGE NoGeneralizedNewtypeDeriving #-}
-- setting the "warn-incomplete-patterns" flag asks GHC to warn you
-- about possible missing cases in pattern-matching definitions
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module Test1
  ( duplicateNums,
    int2digits,
    superEven,
    areTreeAnagrams,
    backPack,
    cross,
    sieveFrom,
    sequenceFrom,
  )
where

import Types

---------------------------------------------------------------------------------
---------------- DO **NOT** MAKE ANY CHANGES ABOVE THIS LINE --------------------
---------------------------------------------------------------------------------

---------------------------------------------------------------------------------
-- QUESTION 1
---------------------------------------------------------------------------------

duplicateNums :: String -> String
duplicateNums cs = foldl (\acc c -> acc ++ if isDigit c then [c, c] else [c]) "" cs

---------------------------------------------------------------------------------
-- QUESTION 2
---------------------------------------------------------------------------------

int2digits :: Int -> [Int]
int2digits n = helper [] n
  where
    helper :: [Int] -> Int -> [Int]
    helper ds x | x < 10 = x : ds
    helper ds x =
      let rem = x `mod` 10
          lower = x `div` 10
       in helper (rem : ds) lower

superEven :: Int -> Bool
superEven n
  | n < 0 = False
  | otherwise = all even (int2digits n)

---------------------------------------------------------------------------------
-- QUESTION 3
---------------------------------------------------------------------------------

areTreeAnagrams :: (Eq a) => Tree a -> Tree a -> Bool
areTreeAnagrams (Leaf x) (Leaf y) = x == y
areTreeAnagrams (Branch l1 r1) (Branch l2 r2) =
  (areTreeAnagrams l1 l2 && areTreeAnagrams r1 r2)
    || (areTreeAnagrams l1 r2 && areTreeAnagrams r1 l2)
areTreeAnagrams _ _ = False

---------------------------------------------------------------------------------
-- QUESTION 4
---------------------------------------------------------------------------------

backPack :: (Weighable a, Eq a, Enum a, Bounded a) => Int -> [[a]]
backPack w
  | w <= 0 = [[]]
  | otherwise =
      [ x : xs | x <- enumFromTo minBound maxBound, weight x <= w, xs <- backPack (w - weight x)
      ]
        ++ [[]]

---------------------------------------------------------------------------------
-- QUESTION 5
---------------------------------------------------------------------------------

cross :: Int -> [Bool] -> [Bool]
cross n xs = undefined

sieveFrom :: Int -> [Bool] -> [Bool]
sieveFrom n xs = undefined

sequenceFrom :: Int -> [Bool] -> [Int]
sequenceFrom n xs = undefined
