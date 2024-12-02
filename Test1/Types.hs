-- setting the "warn-incomplete-patterns" flag asks GHC to warn you
-- about possible missing cases in pattern-matching definitions
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

-- see https://wiki.haskell.org/Safe_Haskell
{-# LANGUAGE NoGeneralizedNewtypeDeriving, Safe #-}

---------------------------------------------------------------------------------
-------------------------- DO **NOT** MODIFY THIS FILE --------------------------
---------------------------------------------------------------------------------

module Types (module Types, module Data.Char) where

import Data.Char

---------------------------------------------------------------------------------
-- QUESTION 3
---------------------------------------------------------------------------------

string1 :: String
string1 = "Hello, World"

string2 :: String
string2 = "I ate 10 bananas in 5 minutes"

string3 :: String
string3 = "The clock says 12:34"

---------------------------------------------------------------------------------
-- QUESTION 3
---------------------------------------------------------------------------------

data Tree a = Leaf a
            | Branch (Tree a) (Tree a)
  deriving Show

tree1 :: Tree Int
tree1 = Branch (Branch (Leaf 1) (Leaf 2)) (Branch (Leaf 3) (Leaf 4))

tree2 :: Tree Int
tree2 = Branch (Branch (Leaf 3) (Leaf 4)) (Branch (Leaf 2) (Leaf 1))

---------------------------------------------------------------------------------
-- QUESTION 4
---------------------------------------------------------------------------------

class Weighable a where
  weight :: a -> Int

data Data = A | B | C | D
  deriving(Show, Eq, Enum, Bounded)

instance Weighable Data where
  weight A = 10
  weight B = 20
  weight C = 15
  weight D = 5
