--
--  Lecture2.hs
--
-- Composing functions
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Redundant if" #-}
{-# HLINT ignore "Redundant guard" #-}
{-# HLINT ignore "Use init" #-}
xor :: Bool -> Bool -> Bool
xor x y = (x && not y) || (y && not x)

removeLast :: [a] -> [a]
removeLast xs = reverse (tail (reverse xs))

removeElem :: Int -> [a] -> [a]
removeElem n xs = removeLast (take n xs) ++ drop n xs

-- If-then-else
longLstMsg :: [a] -> String
longLstMsg xs =
  if length xs > 10
    then
      "A long list"
    else
      "A short list"

longLstMsg' :: [a] -> String
longLstMsg' xs | length xs > 10 = "A long list"
longLstMsg' xs | otherwise = "A short list"

longLstMsgFunny :: [a] -> Bool
longLstMsgFunny xs =
  if length xs > 10
    then
      True
    else
      False

-- pattern matching

-- booleans
notB :: Bool -> Bool
notB True = False
notB False = True

andB :: Bool -> Bool -> Bool
andB True True = True
andB True False = False
andB False True = False
andB False False = False

andB' :: Bool -> Bool -> Bool
andB' True True = True
andB' _ _ = False

-- pairs
swap :: (a, b) -> (b, a)
swap (x, y) = (y, x)

mid :: (a, b, c) -> b
mid (_, x, _) = x

-- lists
head' :: [a] -> a
head' [] = error "empty list"
head' (x : _) = x

sndElem :: [a] -> a
sndElem [] = error "empty list"
sndElem (x : []) = error "only one element"
sndElem (_ : x : _) = x

zip' :: ([a], [b]) -> [(a, b)]
zip' ([], _) = []
zip' (_, []) = []
zip' (l : ls, r : rs) = (l, r) : (zip' (ls, rs))

-- λ-expressions
addSeven :: Int -> Int
addSeven x = x + 7

addSeven' :: Int -> Int
addSeven' = \x -> x + 7

flipAll :: [Bool] -> [Bool]
flipAll xs = map (\x -> not x) xs

addSevenToAll :: [Int] -> [Int]
addSevenToAll xs = map (\x -> x + 7) xs