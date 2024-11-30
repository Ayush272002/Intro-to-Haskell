--
--  Lecture2.hs
--

--
-- Some type expressions
--
-- Int
-- Bool
-- String
-- [String]
-- [Bool]
-- [[(Int,Char)]]
-- Int -> Bool
-- (Bool -> Bool,[(Char,Int -> Bool)])

-- Some *polymorphic* type expressions
-- I.e., containing a varialbe "abstract" type

-- [a]
-- a -> b
-- [a] -> a
-- (a,b) -> a
-- [a] -> [a]

-- projections from tuples are polymorphic
fst :: (a, b, c) -> a
fst (x, y, z) = x

snd :: (a, b, c) -> b
snd (x, y, z) = y

thd :: (a, b, c) -> c
thd (x, y, z) = z

-- Example of multiple arguments and currying
example :: Bool -> Int -> Int -> Bool
example x y z = x && (y + z > 7)

--
--  In a Java/C like syntax, the above would be written:
--
--    bool example(bool x, int y, int z) { ..... }
--

-- Here is a *curried* version of the above
example' :: (Bool, Int, Int) -> Bool
example' (x, y, z) = x && (y + z > 7)

--
--  A polymorphic and higher-order function
--
filter' :: (a -> Bool) -> [a] -> [a]
filter' p [] = []
filter' p (x : xs) = if p x then x : (filter' p xs) else (filter' p xs)
