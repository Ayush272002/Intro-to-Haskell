-- see https://wiki.haskell.org/Safe_Haskell
{-# LANGUAGE Safe #-}
-- see https://wiki.haskell.org/Safe_Haskell
{-# LANGUAGE NoGeneralizedNewtypeDeriving #-}
-- setting the "warn-incomplete-patterns" flag asks GHC to warn you
-- about possible missing cases in pattern-matching definitions
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module Homework (choose, simulate, cut, shuffle, riffles, permute, genTree) where

import Types

---------------------------------------------------------------------------------
---------------- DO **NOT** MAKE ANY CHANGES ABOVE THIS LINE --------------------
---------------------------------------------------------------------------------

choose :: (PickingMonad m) => [a] -> m a
choose xs
  | null xs = error "empty list"
  | otherwise = do
      idx <- pick 0 (length xs - 1)
      return (xs !! idx)

simulate :: (Monad m) => m Bool -> Integer -> m Integer
simulate bs n
  | n <= 0 = return 0
  | otherwise = do
      result <- bs
      rest <- simulate bs (n - 1)
      return (if result then rest + 1 else rest)

cut :: (PickingMonad m) => [a] -> m ([a], [a])
cut xs = do
  let len = length xs
  i <- pick 0 len
  return (take i xs, drop i xs)

shuffle :: (PickingMonad m) => ([a], [a]) -> m [a]
shuffle ([], zs) = return zs
shuffle (ys, []) = return ys
shuffle (y : ys, z : zs) = do
  let lenY = length ys + 1
      lenZ = length zs + 1
  chooseHead <- pick 1 (lenY + lenZ)
  if chooseHead <= lenY
    then (y :) <$> shuffle (ys, z : zs)
    else (z :) <$> shuffle (y : ys, zs)

riffles :: (PickingMonad m) => ([a] -> m ([a], [a])) -> (([a], [a]) -> m [a]) -> Int -> [a] -> m [a]
riffles _ _ 0 xs = return xs
riffles cf sf n xs = do
  (ys, zs) <- cf xs
  shuffled <- sf (ys, zs)
  riffles cf sf (n - 1) shuffled

permute :: (PickingMonad m) => [a] -> m [a]
permute [] = return []
permute (x : xs) = do
  rest <- permute xs
  i <- pick 0 (length rest)
  return (take i rest ++ [x] ++ drop i rest)

genTree :: (PickingMonad m) => [a] -> m (Bin a)
genTree [] = error "genTree: empty list"
genTree [x] = return (L x)
genTree xs = do
  shuffled <- shuffleList xs
  buildTree shuffled

buildTree :: (PickingMonad m) => [a] -> m (Bin a)
buildTree [] = error "buildTree: empty list"
buildTree [x] = return (L x)
buildTree (x : xs) = do
  subtree <- buildTree xs
  insertLeaf x subtree

insertLeaf :: (PickingMonad m) => a -> Bin a -> m (Bin a)
insertLeaf x (L y) = do
  choice <- pick 0 1
  return $ if choice == 0 then B (L x) (L y) else B (L y) (L x)
insertLeaf x (B l r) = do
  choice <- pick 0 1
  if choice == 0
    then return (B (L x) (B l r))
    else do
      newRight <- insertLeaf x r
      return (B l newRight)

shuffleList :: (PickingMonad m) => [a] -> m [a]
shuffleList [] = return []
shuffleList xs = do
  idx <- pick 0 (length xs - 1)
  let (before, x : after) = splitAt idx xs
  rest <- shuffleList (before ++ after)
  return (x : rest)

canopy :: Bin a -> [a]
canopy (L x) = [x]
canopy (B l r) = canopy l ++ canopy r