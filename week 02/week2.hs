--
--  Lecture2.hs
--

-- Composing functions
xor :: Bool -> Bool -> Bool
xor x y = (x && not y) || (y && not x)

removeLast :: [a] -> [a]
removeLast xs = reverse (tail (reverse xs))

removeElem :: Int -> [a] -> [a]
removeElem n xs = removeLast (take n xs) ++ drop n xs

-- Take n elements from the list, remove the last one, and then append the rest of the list

-- If-then-else
longLstMsg :: [a] -> String
longLstMsg xs =
  if length xs > 10
    then
      "Long list!"
    else
      "Short list!"

longLstMsg' :: [a] -> String
longLstMsg' xs
  | length xs > 10 = "Long list!"
  | otherwise = "Short list!"

--   pattern matching
notB :: Bool -> Bool
notB True = False
notB False = True

andB :: Bool -> Bool -> Bool
andB True True = True
andB True False = False
andB False True = False
andB False False = False

-- pairs
swap :: (a, b) -> (b, a)
swap (x, y) = (y, x)

-- Lists
head' :: [a] -> a
head' [] = error "Empty list"
head' (x : xs) = x

sndElem :: [a] -> a
sndElem [] = error "empty"
sndElem (x : []) = error "only one element"
sndElem (x : y : xs) = y

zip' :: ([a], [b]) -> [(a, b)]
zip' ([], _) = []
zip' (_, []) = []
zip' (l : ls, r : rs) = (l, r) : (zip' (ls, rs))

-- λ-expressions

addSeven :: Int -> Int
addSeven x = 7 + x

addSeven' :: Int -> Int
addSeven' = (\x -> 7 + x)

flipAll :: [Bool] -> [Bool]
flipAll xs = map (\x -> not x) xs

addSevenToAll :: [Int] -> [Int]
addSevenToAll xs = map (\i -> i + 7) xs
