-- | Exercise 2 template.
-- Instructions.
--      1. You are to write your solutions to the exercise in this file. You
--      may add extra functions in this file to implement your solution.
--
--      2. You are to comment your code indicating how you arrived at a
--      solution.
--
--      3. It is best to avoid Prelude and library functions not already
--      imported and develop your own code for your solutions.
--
--      4. Do NOT change the type or name of any of the template functions.
--
--      5. Do NOT change the name of this file.
--
--      6. Do NOT change the module name of this file.
--
--      7. To submit this to GradeScope, submit ONLY this file.
--
--      8. Have lots of fun :). Wahoo!!
module Ex2 where

-- This imports the required types and support functions for the questions.
import Ex2Types

-- This imports some standard Prelude functions for you to use.
import Prelude (Int, Float, Integer, Eq, Ord, Bool (..), Double, String, otherwise, Num, Integral, Fractional, abs, (+), (-), (^), subtract, (*), (/), signum, (==), (/=), (<), (<=), (>), (>=), compare, (||), (&&), not, rem, mod, div, quot, max, min, fromIntegral, toInteger, undefined, error, Show, show, Bounded, minBound, maxBound, seq)
import Debug.Trace

-- My foldl with slightly bad typing
foldl :: (b -> a -> b) -> b -> [a] -> b
foldl _ x [] = x
foldl f x (y:ys) = (foldl f (f x y) ys)

-- My foldr with slightly bad typing
foldr :: (a -> b -> b) -> b -> [a] -> b
foldr _ x [] = x
foldr f x (y:ys) = y `f` (foldr f x ys)

map :: (a -> b) -> [a] -> [b]
map _ [] = []
map f (a:as) = f a : map f as

filter :: (a -> Bool) -> [a] -> [a]
filter f = foldr (\x acc -> if f x then x : acc else acc) []

(++) :: [a] -> [a] -> [a]
(++) [] b = b
(++) (x:xs) b = x:(++) xs b


-- | Q1.
-- We do a fold because its cool...
twoTautology :: ((Bool, Bool) -> Bool) -> Bool
twoTautology f = foldl (\val pair -> (val && f pair)) True truth_pairs
    where truth_pairs = [(a,b) | a <- [True, False], b <- [True, False]]

-- Two logical formulas are equivalent if the statement of them being equal is a
-- tautology
twoEquiv :: ((Bool, Bool) -> Bool) -> ((Bool, Bool) -> Bool) -> Bool
twoEquiv f g = twoTautology (\pair -> ((f pair) == (g pair)))

-- | Q2
badFermat :: Integer
badFermat = undefined

-- | Q3
collatzIndex :: Int -> SF [Int]
collatzIndex = undefined

-- | Q4
bisection :: (Double -> Double) -> (Double, Double) -> SF Double
bisection = undefined

-- | Q5
bsort :: (a -> a -> Bool) -> [a] -> [a]
bsort = undefined

-- TODO: #1 Redefine using folds
qsort :: (a -> a -> Bool) -> [a] -> [a]
qsort _ [] = []
qsort f (a:as) =
    let least = qsort f (filter (a `f`) as)
        most = qsort f (filter (`f` a) as)
    in least ++ [a] ++ most

msort :: (a -> a -> Bool) -> [a] -> [a]
msort = undefined

-- | Q6
transpose :: Matrix a -> SF (Matrix a)
transpose = undefined

addMat :: DoubleMatrix -> DoubleMatrix -> SF DoubleMatrix
addMat = undefined

multMat :: DoubleMatrix -> DoubleMatrix -> SF DoubleMatrix
multMat = undefined

-- | Q7.
nreverse :: [a] -> [a]
nreverse = undefined

freverse :: [a] -> [a]
freverse = undefined

hreverse :: [a] -> [a]
hreverse = undefined

-- | Q8.
isAVL :: Ord a => STree a -> Bool
isAVL = undefined

-- | Q9.
all_paths :: Rose a -> [[a]]
all_paths = undefined

-- | Q10.
factorialOf1891 :: Integer
factorialOf1891 = undefined

-- | Q11.
widthRose :: Rose a -> Int
widthRose = undefined
