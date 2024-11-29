notB :: Bool -> Bool
notB False = True
notB True = False

swap :: (a, b) -> (b, a)
swap (x, y) = (y, x)

isEmpty :: [a] -> Bool
isEmpty [] = True
isEmpty _ = False

-- and operator
andB :: Bool -> Bool -> Bool
andB True True = True
andB _ _ = False

-- or operator
orB :: Bool -> Bool -> Bool
orB False False = False
orB _ _ = True

-- non exhaustive pattern matching
isTrue :: Bool -> Bool
isTrue True = True

isTrue' :: Bool -> Bool
isTrue' True = True
isTrue' False = error "Not True"

-- on tuples
frst :: (a, b) -> a
frst (x, y) = x

scnd :: (a, b) -> b
scnd (x, y) = y

thrd :: (a, b, c) -> c
thrd (x, y, z) = z

-- add vectors
addVectors :: (Num a, Fractional b) => (a, b) -> (a, b) -> (a, b)
addVectors (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

-- on lists
isEmpty' :: [a] -> Bool
isEmpty' [] = True
isEmpty' (x : xs) = False

isEmpty'' :: [a] -> Bool
isEmpty'' [] = True
isEmpty'' (_ : _) = False

scnd' :: [a] -> a
scnd' (_ : x : _) = x