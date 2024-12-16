-- see https://wiki.haskell.org/Safe_Haskell
{-# LANGUAGE Safe #-}
-- setting the "warn-incomplete-patterns" flag asks GHC to warn you
-- about possible missing cases in pattern-matching definitions
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module ClassTest1 (checkParity, substitution, largestPrimeBetween, strongPrimes, executeCommands, atmChange) where

import Data.Char
import Types

---------------------------------------------------------------------------------
---------------- DO **NOT** MAKE ANY CHANGES ABOVE THIS LINE --------------------
---------------------------------------------------------------------------------

{- Question 1 -}

checkParity :: String -> Bool
checkParity xs
  | count1s xs `mod` 2 == 0 = True
  | otherwise = False
  where
    count1s :: String -> Int
    count1s xs = foldl (\acc x -> if x == '1' then acc + 1 else acc) 0 xs

{- Question 2 -}

substitution :: String -> String -> String
substitution plaintext key = [convert x | x <- plaintext]
  where
    convert :: Char -> Char
    convert x
      | isLower x = toLower (key !! (ord x - ord 'a'))
      | isUpper x = toUpper (key !! (ord x - ord 'A'))
      | otherwise = x

{- Question 3 -}

largestPrimeBetween :: Int -> Int
largestPrimeBetween n = last ([p | p <- [n .. 2 * n], isPrime p])

strongPrimes :: Int -> [Int]
strongPrimes n = take n [x | x <- [3 ..], isPrime x && x > ((prev x + next x) `div` 2)]
  where
    prev :: Int -> Int
    prev x
      | x == 2 = undefined
      | isPrime (x - 1) = x - 1
      | otherwise = prev (x - 1)

    next :: Int -> Int
    next x = head ([x | x <- [(x + 1) ..], isPrime x])

{- Question 4 -}

executeCommands :: [Command] -> (Int, Int) -> (Int, Int)
executeCommands cmnd (x, y) = foldl update (x, y) cmnd
  where
    update :: (Int, Int) -> Command -> (Int, Int)
    update (x, y) (MoveUp, n) = (x, y + n)
    update (x, y) (MoveDown, n) = (x, y - n)
    update (x, y) (MoveLeft, n) = (x - n, y)
    update (x, y) (MoveRight, n) = (x + n, y)

{- Question 5 -}

atmChange :: Int -> [Int] -> [(Int, Int)]
atmChange 0 [] = []
atmChange n [] = undefined
atmChange n ds = (denom, n `div` denom) : atmChange (n `mod` denom) (reverse (tail revds))
  where
    denom = head revds
    revds = reverse ds
