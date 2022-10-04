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

(.) :: (b -> c) -> (a -> b) -> (a -> c)
f . g = \x -> f (g x)

foldavl :: (b -> a -> b -> b) -> b -> STree a -> b
foldavl _ base Leaf = base
foldavl f base (Node lt a rt) = f (foldavl f base lt) a (foldavl f base rt)

foldrose :: (a -> [b] -> b) -> Rose a -> b
foldrose f (RS a ts) = f a (map (foldrose f) ts)



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
collatzIndex n
    | n <= 0 = FF
    | otherwise = SS (collatzIndex' n)
    where
        collatzIndex' n
            | n == 1 = [1]
        -- Get next collatz interger
            | otherwise = n:(collatzIndex' (collatz n))

-- | Q4
bisection :: (Double -> Double) -> (Double, Double) -> SF Double
bisection f (a,b)
-- Make sure a < b
    | a < b = bisection' f (a,b)
    | a > b = bisection' f (b,a)
    | otherwise = FF
    where
        bisection' f (a,b)
            -- If are interval bounds are too close we cannot proceed
            | (b - a) / 2 < e = FF
            -- If our function value is close to zero we cannot tell it is not zero
            | abs fc < e = SS c
            -- Do the actual bisection step
            | signum fc == signum (f a) = bisection' f (c,b)
            | otherwise = bisection' f (a,c)
            where   c = (a + b) / 2
                    fc = f c

-- | Q5
bsort :: (a -> a -> Bool) -> [a] -> [a]
bsort = undefined

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
nreverse [] = []
nreverse (a:as) = nreverse as ++ [a]

freverse :: [a] -> [a]
freverse as = shunt as []
    where
        shunt :: [a] -> [a] -> [a]
    -- If first list is empty we have shunted everything over
        shunt [] bs = bs
    -- Take first element of first list and shunts it over to second list
        shunt (a:as) bs = shunt as (a:bs)

-- Does the same thing as freverse but with a fold
hreverse :: [a] -> [a]
hreverse = foldl (\acc a -> a:acc) []

-- | Q8.
-- TODO: #2 Use fold
isAVL :: Ord a => STree a -> Bool
isAVL Leaf = True
isAVL (Node lt@(Node _ la _) a rt@(Node _ ra _)) = (la < a) && (a < ra) && (isAVL lt) && (isAVL rt)
isAVL (Node (Node Leaf la Leaf) a Leaf) = (la < a)
isAVL (Node Leaf a (Node Leaf ra Leaf)) = (a < ra)
isAVL _ = False

-- | Q9.
all_paths :: Rose a -> [[a]]
all_paths (RS a []) = [[a]]
all_paths (RS a ts) = map (a:) (foldl (\acc t -> acc ++ (all_paths t)) [] ts)

-- | Q10.
factorialOf1891 :: Integer
factorialOf1891 = undefined

-- | Q11.
widthRose :: Rose a -> Int
widthRose = undefined
