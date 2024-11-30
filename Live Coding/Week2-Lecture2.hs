-- ghc extension to the Haskell standard.
{-# LANGUAGE FlexibleInstances #-}

-- This lecture:
--
--      * Complete the "functions" lecture notes.
--      * Introduce type classes (three lecture notes).

-- Complete lambda

{-

ghci> :type map
map :: (a -> b) -> [a] -> [b]

                      type
                       |
ghci> :type odd        V
odd :: Integral a => a -> Bool
         ^
         |
       type constraint

ghci> map odd [1..10]
[True,False,True,False,True,False,True,False,True,False]

ghci> map (\x -> 10*x) [1..10]
[10,20,30,40,50,60,70,80,90,100]

Here "\" is a one-legged Greek letter "λ" and that occurs in the
"λ-calculus" developed by the mathematician Alonzo Church in the
1930's, and which is the basis of functional programming.

The expression "\x -> 2*x" means "the function which given any input x, returns 2*x"

-}

example0 :: (Num a, Enum a) => a -> [a]
example0 n = map multiplyBy10 [1 .. n]
  where
    multiplyBy10 x = 10 * x

example1 :: Int -> [Int]
example1 n = map (\x -> x * 10) [1 .. n]

example2 :: (Num a, Enum a) => a -> [a]
example2 n = map (\x -> 10 * x) [1 .. n]

-- Operator sections

{-
   (*) :: Num a => a -> a -> a
-}

example3 :: (Num a, Enum a) => a -> [a]
example3 n = map (* 10) [1 .. n]

example4 :: (Fractional a, Enum a) => a -> [a]
example4 n = map (/ 10) [1 .. n]

example5 :: (Fractional a, Enum a) => a -> [a]
example5 n = map (10 /) [1 .. n]

{-

ghci> :type zipWith
zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]

-}

example6 :: (Num a, Enum a) => a -> [a]
example6 n = zipWith (\x y -> x + y) [1 .. n] [100 .. 100 + n]

example6' :: (Num a, Enum a) => a -> [a]
example6' n = zipWith (\x -> \y -> x + y) [1 .. n] [100 .. 100 + n]

example7 :: (Num a, Enum a) => a -> [a]
example7 n = zipWith (+) [1 .. n] [100 .. 100 + n]

-- Case expressions
f :: Int -> [Int]
f n = [1 .. n]

-- Haskell does "type inference" to figure out unsupplied types.

g :: Int -> String
g n = case f n of
  [] -> "empty"
  (x : xs) -> "the head is " ++ show x ++ " and the tail is " ++ show xs

-- Type classes
class Eq' a where
  (===) :: a -> a -> Bool
  (=/=) :: a -> a -> Bool
  x =/= y = not (x === y)
  x === y = not (x =/= y)

instance Eq' Bool where
  True === True = True
  False === False = True
  _ === _ = False

-- The following says: "If equality is available on the type a, then equality is also available on the type [a]"

instance (Eq' a) => Eq' [a] where
  [] === [] = True
  [] === (_ : _) = False
  (x : xs) === (y : ys) = x === y && xs === ys

-- Equality of type a is available *because* we wrote "Eq'a =>" in the
-- instance declaration.

-- A counter example would be [odd,even]===[odd,even]. This doesn't
-- work, because we haven't defined an instance of Eq' for functions
-- (it wouldn't be possible anyway, by Rice's Theorem, as a student
-- explained).

-- Two functions f g : Bool -> a are equal if and only if
-- f True = g True and
-- f False = g False

instance (Eq' a) => Eq' (Bool -> a) where
  f === g = (f True === g True) && (f False === g False)
