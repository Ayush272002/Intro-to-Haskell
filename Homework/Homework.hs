-- see https://wiki.haskell.org/Safe_Haskell
{-# LANGUAGE Safe #-}
-- see https://wiki.haskell.org/Safe_Haskell
{-# LANGUAGE NoGeneralizedNewtypeDeriving #-}
-- setting the "warn-incomplete-patterns" flag asks GHC to warn you
-- about possible missing cases in pattern-matching definitions
{-# OPTIONS_GHC -fwarn-incomplete-patterns -Wno-x-partial #-}

module Test2
  ( isNBranching,
    prune,
    applyNTimes,
    gameOver,
    takeTokens,
    prettyShow,
    parseExpression,
    magicBit,
  )
where

import Types

---------------------------------------------------------------------------------
---------------- DO **NOT** MAKE ANY CHANGES ABOVE THIS LINE --------------------
---------------------------------------------------------------------------------

---------------------------------------------------------------------------------
-- QUESTION 1
---------------------------------------------------------------------------------

checkBranch :: Int -> Rose a -> Bool
checkBranch n (Leaf _) = True
checkBranch n (Branch children) = length children == n && all (checkBranch n) children

isNBranching :: Int -> Rose a -> Bool
isNBranching n t = checkBranch n t

pruneBranches :: Int -> [Rose a] -> [Rose a]
pruneBranches n branches = take n branches

prune :: Int -> Rose a -> Rose a
prune n (Leaf x) = Leaf x
prune n (Branch children) =
  let prunedChildren = pruneBranches n children
      prunedSubtrees = map (prune n) prunedChildren
   in Branch prunedSubtrees

---------------------------------------------------------------------------------
-- QUESTION 2
---------------------------------------------------------------------------------

applyNTimes :: (Monad m) => m a -> (a -> m a) -> Int -> m [a]
applyNTimes mx mf n = solve n mx
  where
    solve 0 mx = do
      x <- mx
      return [x]
    solve n mx = do
      x <- mx
      xs <- solve (n - 1) (mf x)
      return (x : xs)

---------------------------------------------------------------------------------
-- QUESTION 3
---------------------------------------------------------------------------------

gameOver :: NimGame Bool
gameOver = do
  (firstHeap, secondHeap) <- get
  let isOver = firstHeap == 0 && secondHeap == 0
  return isOver

takeTokens :: Int -> Heap -> NimGame ()
takeTokens n h = do
  (firstHeap, secondHeap) <- get
  let newFirstHeap = case h of
        First -> max 0 (firstHeap - n)
        Second -> firstHeap
  let newSecondHeap = case h of
        First -> secondHeap
        Second -> max 0 (secondHeap - n)
  put (newFirstHeap, newSecondHeap)

-- example :: NimGame Bool
-- example = do takeTokens 5 First
--              takeTokens 3 Second
--              takeTokens 1 Second
--              gameOver

---------------------------------------------------------------------------------
-- QUESTION 4
---------------------------------------------------------------------------------

prettyShow :: Expr -> String
prettyShow (E e) = foldr (++) "" (map solve e)
  where
    solve :: Enclosure -> String
    solve e
      | Paren expr <- e = "(" ++ prettyShow expr ++ ")"
      | Bracket expr <- e = "[" ++ prettyShow expr ++ "]"
      | Brace expr <- e = "{" ++ prettyShow expr ++ "}"

parseExpression :: Parser Expr
parseExpression = do
  enclosures <- many parseEnclosure
  return (E enclosures)

parseEnclosure :: Parser Enclosure
parseEnclosure = parseParen <|> parseBracket <|> parseBrace

parseParen :: Parser Enclosure
parseParen = do
  char '('
  expr <- parseExpression
  char ')'
  return (Paren expr)

parseBracket :: Parser Enclosure
parseBracket = do
  char '['
  expr <- parseExpression
  char ']'
  return (Bracket expr)

parseBrace :: Parser Enclosure
parseBrace = do
  char '{'
  expr <- parseExpression
  char '}'
  return (Brace expr)

---------------------------------------------------------------------------------
-- QUESTION 5
---------------------------------------------------------------------------------

magicBit :: Cont Int Bool
magicBit = Cont magicHelper

magicHelper :: (Bool -> Int) -> Int
magicHelper k = chooseBestBranch k

chooseBestBranch :: (Bool -> Int) -> Int
chooseBestBranch k =
  let trueResult = evaluateBranch k True
      falseResult = evaluateBranch k False
   in selectBranch trueResult falseResult k

evaluateBranch :: (Bool -> Int) -> Bool -> Int
evaluateBranch k branch = k branch

selectBranch :: Int -> Int -> (Bool -> Int) -> Int
selectBranch trueResult falseResult k
  | trueResult > falseResult = trueResult
  | otherwise = falseResult

-- magicEx1 :: Cont Int Int
-- magicEx1 = do
--   b0 <- magicBit
--   if b0
--     then
--       return 100
--     else
--       return 0

-- magicEx2 :: Cont Int Int
-- magicEx2 = do
--   b0 <- magicBit
--   b1 <- magicBit
--   if b0
--     then
--       if b1
--         then return 20
--         else return 10
--     else
--       if b1
--         then return 50
--         else return 30

-- magicEx3 :: Cont Int Int
-- magicEx3 = do
--   b1 <- magicBit
--   b2 <- magicBit
--   if b1 && not b2
--     then return 100
--     else return 0
-- magicEx4 :: Cont Int Int
-- magicEx4 = do
--   bits <- replicateM 4 magicBit
--   if all id bits
--     then return 60
--     else return 0
