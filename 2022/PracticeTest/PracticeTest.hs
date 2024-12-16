-- see https://wiki.haskell.org/Safe_Haskell
{-# LANGUAGE Safe #-}
-- setting the "warn-incomplete-patterns" flag asks GHC to warn you
-- about possible missing cases in pattern-matching definitions
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module PracticeTest (checksum, golfScorer, highlyDivisible, largestOddFactor, equals, babylonianPalindromes) where

import Types

---------------------------------------------------------------------------------
---------------- DO **NOT** MAKE ANY CHANGES ABOVE THIS LINE --------------------
---------------------------------------------------------------------------------

{- Question 1 -}
checksum :: (Integral a) => [a] -> Bool
checksum as
  | length as < 8 = False
  | calcSum as `mod` 11 == 0 = True
  | otherwise = False
  where
    calcSum :: (Integral a) => [a] -> a
    calcSum as = foldl (\x acc -> acc + x) 0 as

{- Question 2 -}
golfScorer :: Integer -> Integer -> Integer
golfScorer a b
  | b == 1 = 5
  | a - b >= 2 = 4
  | a - b == 1 = 3
  | a - b == 0 = 2
  | a - b == -1 = 1
  | otherwise = 0

{- Question 3 -}
highlyDivisible :: Int -> [Int]
highlyDivisible n = take n [i | i <- [1 ..], allDivides i]
  where
    allDivides :: Int -> Bool
    allDivides n = all (\k -> n `mod` k == 0) [2 .. 12]

largestOddFactor :: Int -> [Int]
largestOddFactor n = solve (factors n)
  where
    solve :: [Int] -> [Int]
    solve xs = [head (reverse (filter odd xs))]

{- Question 4 -}
equals :: (Enum a, Bounded a, Eq b) => (a -> b) -> (a -> b) -> Bool
equals f g = and [f x == g x | x <- [minBound .. maxBound]]

{- Question 5 -}

base60 :: Integer -> [Integer]
base60 n = solve n []
  where
    solve :: Integer -> [Integer] -> [Integer]
    solve x xs
      | x < 60 = x : xs
      | otherwise =
          let r = n `mod` 60
              m = n - r
           in solve (m `div` 60) (r : xs)

babylonianPalindromes :: [Integer]
babylonianPalindromes = [i | i <- [1 ..], let b = base60 i, length b > 1, b == reverse b]
