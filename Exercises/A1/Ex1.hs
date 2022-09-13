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
import Prelude (Int, Float, Integer, Eq, Ord, Bool (..), String, otherwise, abs, (+), (-), subtract, (*), (/), (==), (/=), (<), (<=), (>), (>=), (||), (&&), rem, mod, div, quot, max, min, fromIntegral, undefined, error, show)
-- This includes helpful functions for debugging.
import Debug.Trace
import Control.Exception (TypeError)
import Distribution.Simple.Utils (xargs)

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

-- cat :: [a] -> [a] -> [a]
-- cat [] b = b
-- cat (xxs) b

-- quicksort :: (Ord a) => [a] -> [a]
-- quicksort [] = []
-- quicksort (x:xs) =
--     let least = quicksort [a | a <- xs, a <= x]
--         most = quicksort [a | a <- xs, a > x]
--     in least:x

-- | Q1.
avgThree :: Int -> Int -> Int -> Float
avgThree a b c = avg [a,b,c]

-- | Q2.
maxThree :: Int -> Int -> Int -> (Int, Int)
maxThree a b c = let m = max' [a,b,c] in (m, sum [1 | x <- [a,b,c], x == m])

-- | Q3.
notlog :: Integer -> Integer
notlog n
    | n <= 1 = 0
    | otherwise = 1 + notlog (n `quot` 2)

invExp :: Integer -> SF Integer
invExp n
    | n <= 0 = FF
    | otherwise = SS (notlog n)

-- | Q4.
myLcm :: Int -> Int -> Int
myLcm a b = abs(a * b) `quot` (gcd a b)

-- | Q5.
binom :: Integer -> Integer -> Integer
binom = undefined

-- | Q6.
grow :: String -> String
grow = undefined

-- | Q7.
instrictorder :: [Int] -> Bool
instrictorder [] = True
instrictorder (x:[]) = True
instrictorder (x:y:xs) = (x > y) && instrictorder xs

-- | Q8.
expensive :: [(String, Int)] -> Int -> [String]
expensive lst cost = [fst pair | pair <- lst, snd pair > cost]

-- | Q9.
sortCheapest :: [(String, Int)] -> [(String, Int)]
sortCheapest = undefined

-- | Q10.
-- divisors :: Integer -> [Integer]
-- divisors n
--     | n <= 0 = []
--     | otherwise = [m | m <- [1..n], (n `mod` m) == 0, ]

-- | Q11.
substring :: String -> String -> Bool
substring = undefined

-- | Q12.
sublists :: [a] -> [[a]]
sublists = undefined
