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

import Prelude (Int, Float, Integer, Eq, Ord, Bool (..), Double, String, otherwise, Num, Integral, Fractional, abs, (+), (-), (^), subtract, (*), (/), signum, (==), (/=), (<), (<=), (>), (>=), compare, (||), (&&), not, rem, mod, div, quot, max, min, fromIntegral, toInteger, undefined, error, Show, show, Bounded, minBound, maxBound, seq)
import Debug.Trace


-- General Useful Functions
-- These are mostly just standard function
foldl :: (b -> a -> b) -> b -> [a] -> b
foldl _ x [] = x
foldl f x (y:ys) = (foldl f (f x y) ys)

foldr :: (a -> b -> b) -> b -> [a] -> b
foldr _ x [] = x
foldr f x (y:ys) = y `f` (foldr f x ys)

map :: (a -> b) -> [a] -> [b]
map _ [] = []
map f (a:as) = f a : map f as

filter :: (a -> Bool) -> [a] -> [a]
filter f = foldr (\x acc -> if f x then x : acc else acc) []

length :: [a] -> Int
length [] = 0
length (a:as) = 1 + length as

fst :: (a,b) -> a
fst (a,_) = a

snd :: (a,b) -> b
snd (_,b) = b

head :: [a] -> a
head [] = error "Error: empty list"
head (a:as) = a

-- Takes first n elements of a list
take :: Int -> [a] -> [a]
take _ [] = []
take n (a:as)
    | n == 0    = []
    | otherwise = a : take (n-1) as

drop :: Int -> [a] -> [a]
drop _ [] = []
drop n at@(a:as)
    | n == 0    = at
    | otherwise = drop (n-1) as

sum :: (Num a) => [a] -> a
sum = foldr (+) 0

product :: (Num a) => [a] -> a
product = foldl (*) 1

(++) :: [a] -> [a] -> [a]
(++) [] b = b
(++) (x:xs) b = x:(++) xs b

(.) :: (b -> c) -> (a -> b) -> (a -> c)
f . g = \x -> f (g x)

(!!) :: [a] -> Int -> a
(!!) [] _ = error "Error: Index too large"
(!!) (a:as) n
    | n==0      = a
    | n < 0     = error "Error: Negative index"
    | otherwise = (!!) as (n-1)

id :: a -> a
id a = a

zip :: [a] -> [b] -> [(a,b)]
zip [] _ = []
zip _ [] = []
zip (a:as) (b:bs) = (a,b):zip as bs

uncurry :: (a -> b -> c) -> ((a,b) -> c)
uncurry f (a,b) = f a b

-- Q2 Auxillary
isPrime :: Integer -> Bool
isPrime n
    | n < 0         = error "Error: Number must be positive"
    | n <= 1        = False
    | mod n 2 == 0  = False
    | otherwise     = isPrime' 3 n
    where -- Perform brute force search of all values less that root n with step size 2
        isPrime' i n
            | i*i > n       = True
            | mod n i == 0  = False
            | otherwise     = isPrime' (i+2) n

-- Q5 Auxillary
-- Merge two sorted lists into a sorted list
merge :: (Ord a) => [a] -> [a] -> [a]
merge [] bs = bs
merge as [] = as
merge at@(a:as) bt@(b:bs) = if a <= b then a:(merge as bt) else b:(merge at bs)

-- Merges a list of ordered lists. Idea is we merge pairs of lists in our list
-- then recurse.
mergeall :: (Ord a) => [[a]] -> [a]
mergeall []     = []
mergeall [a]    = a
mergeall xs     = mergeall (mergeall' xs)
    where
        -- Helper for mergeall.
        mergeall' (a:b:t)   = merge a b : mergeall' t
        mergeall' t         = t -- singleton and empty case

-- Q6 Auxillary
-- Does map over two lists
dmap :: (a -> b -> c) -> [a] -> [b] -> [c]
dmap f as bs = map (uncurry f) (zip as bs)

-- Checks if all values of list are equal
alleq :: (Eq a) => [a] -> a -> Bool
alleq as a = foldl (\acc x -> acc && (x == a)) True as

-- Q8 Auxillary
foldexpr :: (f -> [c] -> c) -> (x -> c) -> (Expr f x) -> c
foldexpr _ h (Var x) = h x
foldexpr g h (Fun f as) = g f (map (foldexpr g h) as)

-- Q9 Auxillary
-- factorial from http://www.willamette.edu/~fruehr/haskell/evolution.html
fac :: (Integral n) => n -> n
fac n = product [1..n]

-- Q10 Auxillary
-- fold for rose tree by doing the fold thing
foldrose :: (a -> [b] -> b) -> Rose a -> b
foldrose f (RS a ts) = f a (map (foldrose f) ts)

-- TODO: #3 Make this not terrible
maxk :: (Ord a) => [a] -> Int -> [a]
maxk as n = take n (hreverse (msort as))

-- Q11 Auxillary
foldavl :: (b -> a -> b -> b) -> b -> STree a -> b
foldavl _ base Leaf = base
foldavl f base (Node lt a rt) = f (foldavl f base lt) a (foldavl f base rt)


-- 1
-- Try all pairs of Bools with a fold
twoTautology :: ((Bool, Bool) -> Bool) -> Bool
twoTautology f = foldl (\val pair -> (val && f pair)) True truth_pairs
    where truth_pairs = [(a,b) | a <- [True, False], b <- [True, False]]

-- Two logical formulas are equivalent if the statement of them being equal is
-- always true i.e. a tautology
twoEquiv :: ((Bool, Bool) -> Bool) -> ((Bool, Bool) -> Bool) -> Bool
twoEquiv f g = twoTautology (\pair -> ((f pair) == (g pair)))


--2
-- We use foldr to make a list of all values that break Fermat's conjecture and
-- then take the first which works thanks to Haskells laziness
badFermat :: Integer
badFermat = (foldr (\n acc -> if isPrime (2^(2^n) + 1) then acc else n:acc) [] [1..]) !! 0


-- 3
collatzIndex ::  Int -> SF [Int]
collatzIndex n
    | n <= 0        = FF -- Negative intergers do not have collatz indeces
    | otherwise     = SS (collatzIndex' n)
    where
        collatzIndex' n -- Get next collatz interger
            | n == 1        = [1]
            | otherwise     = n:(collatzIndex' (collatz n))


-- 4
bisection :: (Double -> Double) -> (Double, Double) -> SF Double
bisection f (a,b)
-- Make sure a < b
    | a < b = bisection' f (a,b)
    | a > b = bisection' f (b,a)
    | otherwise = FF
    where
        bisection' f (a,b)
            -- If our function value is close to zero we cannot tell it is not zero
            | abs fc < e = SS c
            -- If our interval bounds are too close we cannot proceed
            | (b - a) / 2 < e = FF
            -- Do the actual bisection step
            | signum fc == signum (f a) = bisection' f (c,b)
            | otherwise                 = bisection' f (a,c)
            where   c = (a + b) / 2
                    fc = f c


-- 5
bsort:: (Ord a) => [a] -> [a]
bsort as = foldl (\acc _ -> bubble acc) as as
    where
        bubble (a:b:as) -- Inner bubble operation
            | a > b     = b : bubble (a:as)
            | otherwise = a : bubble (b:as)
        bubble a = a -- Singleton and empty case

qsort :: (Ord a) => [a] -> [a]
qsort [] = []
qsort (a:as) =
    let least = qsort (filter (a >=) as)-- Get the elements smaller than a and sort
        most = qsort (filter (a <) as)  -- Get the elements larger than a and sort
    in least ++ [a] ++ most -- add up

-- Adapted from https://hackage.haskell.org/package/base-4.17.0.0/docs/src/Data.OldList.html#sort
-- The idea is that we split the list into increasing lists then merge which takes
-- advantage of partial ordered sublists instead of spliting naively
msort :: (Ord a) => [a] -> [a]
msort = mergeall . split
    where
        -- Creates list of increasing  lists
        split (a:b:xs)
            | a > b     = decreasing b [a] xs
            | otherwise = increasing b (a:) xs
        split xs = [xs] -- singleton and empty case
        -- Creates increasing list for decreasing numbers
        decreasing a as (b:bs)
            | a > b         = decreasing b (a:as) bs
        decreasing a as bs  = (a:as): split bs
        -- Creates increasing list for increasing numbers
        increasing a as (b:bs)
            | a <= b        = increasing b (\ys -> as (a:ys)) bs
        increasing a as bs   = as [a]: split bs


-- 6
-- Get length of each row in matrix and make sure they agree then return our dims
dimension :: Matrix a -> (SF (Int,Int))
dimension mat
    | alleq dims (head dims)    = SS (length dims, dims !! 0) -- Make sure each row has same dim
    | otherwise                 = FF
    where dims = map length mat -- Get dim of each row

-- We make an endless list of empty lists, then use our double map (dmap) function to
-- add each element of each row (using a foldr) to one of the empty lists. Our
-- very long list gets truncated by dmap.
transpose :: Matrix a -> (SF (Matrix a))
transpose mat = case dimension mat of
    FF          -> FF -- Make sure mat is not ragged
    SS (n,m)    -> SS (foldr (dmap (:)) i mat)
        where i = []:i

-- We can use our dmap function to map addition across both matrices.
addMat :: DoubleMatrix -> DoubleMatrix -> (SF DoubleMatrix)
addMat m1 m2 = case (dimension m1, dimension m2) of
    -- Check that matrices are not bad
    (FF, _) -> FF
    (_, FF) -> FF
    (d1, d2)
        | d1 == d2  -> SS (dmap (dmap (+)) m1 m2) -- Do actual addition
        | otherwise -> FF

multMat :: DoubleMatrix -> DoubleMatrix -> (SF DoubleMatrix)
multMat m1 m2 = case (dimension m1, dimension m2) of
    -- Check that matrices are not bad
    (SS (_,n1), SS (n2,_))
        | n1 == n2  -> SS (multMat' m1 m2)
        | otherwise -> FF
    _ -> FF
    where
        -- The function that does the actual matrix multiplication. Idea is we
        -- can transpose the second matrix and then fold over both mats to create
        -- the result.
        multMat' m1 m2 = foldr dotprod [] m1
            where
                dotprod row newmat = (foldr (\col newrow -> (sum (dmap (*) row col)):newrow) [] m2t):newmat
                m2t = case transpose m2 of FF -> [[]]; (SS m) -> m

-- 7
-- The naive way
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

-- Instead of using foldl we use foldr to build up a function as our fold variable
-- which returns the reversed list
hreverse :: [a] -> [a]
hreverse as = foldr (\a f -> (\x -> f(a:x))) id as []



-- 8
--
all_paths :: Expr f x -> [([f],x)]
all_paths = foldexpr (\f css -> foldr (\cs acc -> map (add_fst f) cs ++ acc) [] css) (\x -> [([], x)])
    where
        -- Appends element to list in first idx of tuple
        add_fst a (as, b) = ((a:as), b)
-- Could also use the foldless and pointy version below:
-- all_paths (Var x) = [([],x)]
-- all_paths (Fun f fs) = map (add_fst f) (foldl (\acc t -> acc ++ (all_paths t)) [] fs)


-- 9
fact :: Integer
fact = fac 1891


-- 10
-- We can fold over all nodes in our tree and seeing if we can add sub-branches
-- of the node to generate a longer path than our current longest path.
widthRose :: Integral a =>  Rose a -> Int
widthRose t = snd (foldrose widthroot t) -- Longest path is stored in second
    where
        -- The tuples store (longest path through node, longest path see)
        widthroot _ [] = (0, 0)
        -- Update.
        widthroot _ cs = (1 + foldr max 0 branches, max path curWidth)
            where
                branches = maxk (map fst cs) 2  -- Take the two largest sub-branchses
                path = sum branches + length branches   -- Generate longest path through node
                curWidth = foldr max 0 (map snd cs) -- Get the current longest path


-- 11
datas Value a = Bot | Val a | Top
    deriving (Show, Eq, Ord)

-- We can use higher order folds to check the ordering at each node level while
-- avoiding unpleasantries of the leaves, and everything works efficiently thanks
-- to laziness!
is_ordered_bad :: (Ord a) => STree a -> Bool
is_ordered_bad tr@(Node _ a _) = both ((foldavl order (\_ -> (True, True)) tr ) a)
    where
        order lf a rf = (\x -> ((a <= x) && ordBelow, (x <= a) && ordBelow))
            where
                ordBelow = fst (lf a) && snd (rf a)
        both (a,b) = a && b

is_ordered :: (Ord a) => STree a -> Bool
is_ordered Leaf = True
is_ordered tr = is_ordered' tr
    where
        is_ordered'


-- Uses SF to see if tree is balanced
is_balanced :: STree a -> Bool
is_balanced tr = case status of (SS _) -> True
                                _ -> False
    where
        status = foldavl balanced (SS 0) tr

        balanced (SS ld) _ (SS rd)
            -- Do the balance check, it it fails we bubble up a failure
            | abs(ld - rd) <= 1 = (SS (1 + max ld rd))
            | otherwise         = FF
        balanced _ _ _          = FF



strong_balance ::  STree a -> STree a
strong_balance = balance_2

-- Turns binary tree into ordered list
ordTree2List :: (STree a) -> [a]
ordTree2List t = (foldavl (\lt a rt -> (lt).(\x -> a:x).(rt)) id t) []

-- We can break up an ordered binary tree into an ordered list in O(n) time then
-- build it backup into a balanced binary tree in O(n) time.
balance_1 :: STree a -> STree a
balance_1 Leaf = Leaf
balance_1 t = balance_1' (ordTree2List t)
    where
        -- Build tree up again
        balance_1' [] = Leaf
        balance_1' lst = (Node (balance_1' left) middle (balance_1' right))
            where
                -- Get left middle and right of list and make into nodes or subtrees
                middle = lst !! midIdx
                midIdx = quot (length lst) 2
                left = take midIdx lst
                right = drop (midIdx+1) lst

-- Rotates root of tree left
rotateLeft :: STree a -> STree a
rotateLeft (Node lt a (Node lrt ar rrt)) = (Node (Node lt a lrt) ar rrt)
rotateLeft tr = tr

-- Rotates root of tree right
rotateRight :: STree a -> STree a
rotateRight (Node (Node llt al rlt) a rt) = (Node llt al (Node rlt a rt))
rotateRight tr = tr

-- Uses binary search tree rotations to balance an AVL tree. Works by balancing
-- the current node then balancing subbranches. Rotation algos are derived from
-- Introduction to Algorithms (3 ed). Cormen et al. The MIT Pres, 2009. Is currently
-- not the most efficient in terms of operations but should be efficient in terms
-- of number of rotations.
balance_2 :: STree a -> STree a
balance_2 Leaf = Leaf
balance_2 tr = (Node (balance_2 lt) a (balance_2 rt))
    where
        (Node lt a rt) = balance_2' tr (0,0)
        balance_2' tr@(Node lt a rt) lr
            | abs (big - sml) <=1    = tr -- Found center
            | big > sml              = rotateLeft (Node lt a (balance_2' rt (lr `tp` (sml+1,0)))) -- Look down right
            | otherwise              = rotateRight (Node (balance_2' lt (lr `tp` (0,big+1))) a rt) -- Look down left
            where
                (sml, big) = (numNodes lt, numNodes rt ) `tp` lr

-- Adds two tuples of numbers
tp :: (Num a, Num b) => (a,b) -> (a,b) -> (a,b)
tp (a1,b1) (a2,b2) = (a1+a2,b1+b2)

-- Counts nodes in a tree
numNodes :: STree a -> Int
numNodes tr = foldavl (\lf _ rf -> (\x -> lf x + 1 + rf x)) id tr 0

