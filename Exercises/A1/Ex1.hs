-- | Exercise 1 template.
-- Instructions.
--      1. Write your solutions to the exercise in this file. You
--      may add extra functions in this file to implement your solution.
--
--      2. Do NOT change the type or name of any of the template functions.
--
--      3. Do NOT change the name of this file.
--
--      4. Do NOT change the module name of this file.
--
--      5. To submit this to GradeScope, submit ONLY this file.
--
--      6. Have lots of fun :)
module Ex1 where

-- This includes the required types for this assignment (namely, the 'SF' type).
import Ex1Types

-- This imports some standard library functions for you to use.
import Prelude (Int, Float, Integer, Eq, Ord, Bool (..), Num, String, otherwise, abs, (+), (-), subtract, (*), (/), (==), (/=), (<), (<=), (>), (>=), (||), (&&), rem, mod, div, quot, max, min, fromIntegral, undefined, error, show)
-- This includes helpful functions for debugging.
import Debug.Trace

head :: [a] -> a
head [] = error "empty list"
head (x:_) = x

tail :: [a] -> a
tail [] = error "empty list"
tail [x] = x
tail (x:xs) = tail xs

sum :: (Num a) => [a] -> a
sum [] = 0
sum (x:xs) = x + sum xs

length :: [a] -> Int
length [] = 0
length (x:xs) = 1 + length xs

repeat :: a -> [a]
repeat a = a:repeat a
repeat' :: a -> Int -> [a]
repeat' _ 0 = []
repeat' a n = a:repeat' a (n-1)

max' :: (Ord a) => [a] -> a
max' [] = error "empty list"
max' [x] = x
max' (x:xs) = max x (max' xs)

numeq :: (Eq m) => [m] -> m -> Int
numeq [] _  = 0
numeq (x:xs) m
    |x == m = 1 + numeq xs m
    |x /= m = 0 + numeq xs m

filter :: (a -> Bool) -> [a] -> [a]
filter _ [] = []
filter f (x:xs)
    | f x = x : filter f xs
    | otherwise = filter f xs

map :: (a -> b) -> [a] -> [b]
map _ [] = []
map f (a:as) = f a : map f as

fst :: (a,b) -> a
fst (a,_) = a

snd :: (a,b) -> b
snd (_,b) = b

gcd :: Int -> Int -> Int
gcd a b
    | b == 0 = a
    | otherwise = gcd b (a `mod` b)

notlog :: Integer -> Integer
notlog n
    | n <= 1 = 0
    | otherwise = 1 + notlog (n `quot` 2)

(++) :: [a] -> [a] -> [a]
(++) [] b = b
(++) (x:xs) b = x:(++) xs b

(.) :: (b -> c) -> (a -> b) -> (a -> c)
f . g = \x -> f (g x)

-- | Q1.
avgThree :: Int -> Int -> Int -> Float
avgThree a b c = fromIntegral a / 3 + fromIntegral b / 3 + fromIntegral c / 3

-- | Q2.
maxThree :: Int -> Int -> Int -> (Int, Int)
maxThree a b c = let m = max' [a,b,c] in (m, numeq [a,b,c] m)

-- | Q3.
invExp :: Integer -> SF Integer
invExp n
    | n <= 0 = FF
    | otherwise = SS (notlog n)

-- | Q4.
myLcm :: Int -> Int -> Int
myLcm a b
    | a==0 && b==0 = 0
    | otherwise = abs(a * b `quot` gcd a b)

-- | Q5.
binom :: Integer -> Integer -> Integer
binom n k
    | k > n = error "Need k < n"
    | n <= 0 || k < 0 = error "Need n >= 1 and k >= 0"
    | k == 0 = 1
    | k > (n `quot` 2) = binom n (n-k)
    | otherwise = n * (binom (n-1) (k-1)) `quot` k

-- | Q6.
grow :: String -> String
grow lst = grow' lst 1
    where   grow' [] _ = []
            grow' (a:as) n = repeat' a n ++ grow' as (n+1)

-- | Q7.
instrictorder :: [Int] -> Bool
instrictorder [] = True
instrictorder [x] = True
instrictorder (x:y:xs) = (x > y) && instrictorder xs

-- | Q8.
expensive :: [(String, Int)] -> Int -> [String]
expensive lst cost =  [fst pair | pair <- lst, snd pair > cost]

-- | Q9.
sortCheapest :: [(String, Int)] -> [(String, Int)]
sortCheapest [] = []
sortCheapest (x:xs) =
    let least = sortCheapest (filter ((<=snd x).snd) xs)
        most = sortCheapest (filter ((>snd x).snd) xs)
    in least ++ [x] ++ most

-- | Q10.
quott :: Integer -> Integer -> Integer
quott n m
    | m==0 = error "Division by zero"
    | mod n m == 0 = quott (quot n m) m
    | otherwise = n

divisors :: Integer -> [Integer]
divisors n
    | n <= 1 = []
    | mod n 2 == 0 = 2:divisors (quott n 2)
    | otherwise = divisors' n 3 where
        divisors' n m
            | n <= 1 = []
            | mod n m == 0 = m:divisors' (quott n m) m
            | otherwise = divisors' n (m+2)

-- | Q11.
headeq :: (Eq a) => [a] -> [a] -> Bool
headeq [] _ = True
headeq _ [] = False
headeq (a:as) (b:bs)
    | a==b = headeq as bs
    | otherwise = False

substring :: String -> String -> Bool
substring [] _ = True
substring _ [] = False
substring at@(a:as) (b:bs)
    | a == b && headeq as bs = True
    | otherwise = substring at bs

-- | Q12.
sublists :: [a] -> [[a]]
sublists [] = [[]]
sublists (a:as) = map (a:) power_as ++ power_as
    where power_as = sublists as