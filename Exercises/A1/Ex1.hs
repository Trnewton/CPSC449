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
-- This imports some standard library functions for you to use.
import Prelude (Int, Float, Integer, Eq, Ord, Bool (..), String, otherwise, abs, (+), (-), subtract, (*), (/), (==), (/=), (<), (<=), (>), (>=), (||), (&&), rem, mod, div, quot, max, min, fromIntegral, undefined, error, show)
-- This includes helpful functions for debugging.
import Debug.Trace
import Control.Exception (TypeError)
import Distribution.Simple.Utils (xargs)
import System.Console.Haskeline (Interrupt)

head :: [a] -> a
head [] = error "empty list"
head (x:_) = x

tail :: [a] -> a
tail [] = error "empty list"
tail [x] = x
tail (x:xs) = tail xs

sum :: [Int] -> Int
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

avg :: [Int] -> Float
avg lst = fromIntegral (sum lst) / fromIntegral(length lst)

max' :: (Ord a) => [a] -> a
max' [] = error "empty list"
max' [x] = x
max' (x:xs) = max x (max' xs)

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

fact :: Integer -> Integer
fact 0 = 1
fact n = n * fact (n-1)

(++) :: [a] -> [a] -> [a]
(++) [] b = b
(++) (x:xs) b = x:(++) xs b

filter :: (a -> Bool) -> [a] -> [a]
filter _ [] = []
filter f (x:xs)
    | f x = x: filter f xs
    | otherwise = filter f xs

-- TODO: Somehow overflows
-- | Q1.
avgThree :: Int -> Int -> Int -> Float
avgThree a b c = avg [a,b,c]

-- | Q2.
maxThree :: Int -> Int -> Int -> (Int, Int)
maxThree a b c = let m = max' [a,b,c] in (m, sum [1 | x <- [a,b,c], x == m])

-- | Q3.
invExp :: Integer -> SF Integer
invExp n
    | n <= 0 = FF
    | otherwise = SS (notlog n)

-- | Q4.
myLcm :: Int -> Int -> Int
myLcm a b = abs(a * b) `quot` gcd a b

-- TODO: Factorial overflows
-- | Q5.
binom :: Integer -> Integer -> Integer
binom 0 _ = 1
binom k n = fact n `quot` (fact k * fact (n-k))

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
expensive lst cost = [fst pair | pair <- lst, snd pair > cost]

-- | Q9.
sortCheapest :: [(String, Int)] -> [(String, Int)]
sortCheapest [] = []
sortCheapest (x:xs) =
    let least = sortCheapest [a | a <- xs, snd a <= snd x]
        most = sortCheapest [a | a <- xs, snd a > snd x]
    in least ++ [x] ++ most

-- TODO: Remove copies
-- | Q10.
divisors :: Integer -> [Integer]
divisors n
    | n <= 1 = []
    | mod n 2 == 0 = 2:divisors (quot n 2)
    | otherwise = divisors' n 3 where
        divisors' n m
            | n <= 1 = []
            | mod n m == 0 = m:divisors' (quot n m) m
            | otherwise = divisors' n (m+2)

-- TODO: Backtracking
-- | Q11.
substring :: String -> String -> Bool
substring _ [] = False
substring [] _ = True
substring at@(a:as) (b:bs)
    | a == b = substring as bs
    | otherwise = substring at bs

-- | Q12.
sublists :: [a] -> [[a]]
sublists [] = [[]]
sublists (a:as) = [a:lst | lst <- power_as] ++ power_as
    where power_as = sublists as