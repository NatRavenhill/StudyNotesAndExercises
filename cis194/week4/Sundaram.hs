module Sundaram where

--1. start with a list of numbers from 0 to n
sieveList n = [0..n]

--2. remove all numbers of the form i + j + 2ij where 1 <= i <=j and i + j + 2ij n
--a) to get all of possible is and js we use the cartesian product function

cartProd :: [a] -> [b] -> [(a,b)]
cartProd xs l =concatMap (\x -> help x l) xs
      where
          help a [] = [] 
          help a (b:bs) = (a,b) : help a bs

---more than 0 and I less than J
allIAndJ :: (Enum a, Num a, Ord a) => a -> [(a, a)]
allIAndJ n = filter (\(x,y) -> x <= y) (cartProd [1..n] [1..n])

--b) Now we need to calculate numbers of the form i + j + 2ij
sundNum :: (Enum a, Num a, Ord a) => (a, a) -> a
sundNum (i,j) = i + j + (2 * i * j)

--make a list satisying this given the pairs, where the elements are <= n
sundNumList n = filter (\x -> x <= n) (map sundNum (allIAndJ n))

--c) Then remove these numbers from the list
--for list of numbers, original list
sieve [] l = l
sieve (x:xs) l = sieve xs (remove x l)

remove :: (Enum a, Eq a, Num a) => a ->[a] -> [a]
remove n l = filter (\x -> x /= n) l

--4) final step!
--with the numbers we have left, do 2n + 1
final l = map (\n -> 2 * n + 1) l

--Therefore our entire process is summed up all primes below 2n+2
sundaram n = final (sieve (sundNumList n) [1..n])
                         
 
