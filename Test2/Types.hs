-- see https://wiki.haskell.org/Safe_Haskell
{-# LANGUAGE Safe #-}
-- see https://wiki.haskell.org/Safe_Haskell
{-# LANGUAGE NoGeneralizedNewtypeDeriving #-}
-- setting the "warn-incomplete-patterns" flag asks GHC to warn you
-- about possible missing cases in pattern-matching definitions
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

---------------------------------------------------------------------------------
-------------------------- DO **NOT** MODIFY THIS FILE --------------------------
---------------------------------------------------------------------------------

module Types (module Types, module Parsing, module Control.Monad, module Control.Monad.State) where

import Control.Monad
import Control.Monad.State
import Parsing

---------------------------------------------------------------------------------
-- QUESTION 1
---------------------------------------------------------------------------------

data Rose a
  = Leaf a
  | Branch [Rose a]
  deriving (Show)

---------------------------------------------------------------------------------
-- QUESTION 2
---------------------------------------------------------------------------------

addAndPrint :: Int -> IO Int
addAndPrint n = do
  putStrLn $ show n
  return $ n + 2

---------------------------------------------------------------------------------
-- QUESTION 3
---------------------------------------------------------------------------------

type NimBoard = (Int, Int)

type NimGame a = State NimBoard a

data Heap = First | Second

---------------------------------------------------------------------------------
-- QUESTION 4
---------------------------------------------------------------------------------

data Enclosure
  = Paren Expr
  | Bracket Expr
  | Brace Expr
  deriving (Show)

data Expr = E [Enclosure]
  deriving (Show)

printEx1 :: Expr
printEx1 = E [Paren (E []), Brace (E []), Bracket (E [])]

printEx2 :: Expr
printEx2 = E [Paren (E [Brace (E [Bracket (E [])])])]

successEx1 :: String
successEx1 = "{([[]]){}()}"

successEx2 :: String
successEx2 = "{()}[({})]"

successEx3 :: String
successEx3 = "({{[[](){}]}})"

successEx4 :: String
successEx4 = ""

failEx1 :: String
failEx1 = "({}"

failEx2 :: String
failEx2 = "()[}]"

---------------------------------------------------------------------------------
-- QUESTION 5
---------------------------------------------------------------------------------

newtype Cont r a = Cont {runCont :: (a -> r) -> r}

instance Functor (Cont r) where
  fmap f m = Cont $ \k -> runCont m (k . f)

instance Applicative (Cont r) where
  pure a = Cont $ \k -> k a
  mf <*> mx = Cont $ \k -> runCont mf (\f -> runCont mx (k . f))

instance Monad (Cont r) where
  m >>= f = Cont $ \k -> runCont m (\a -> runCont (f a) k)
