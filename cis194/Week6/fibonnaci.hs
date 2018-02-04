{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}
module Fibonacci where

 --1-- standard fibonacci function
fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n
  | n >= 2 = fib (n-1) + fib (n-2)

--infinite list of fibs
fibs1 :: [Integer]
fibs1 = map fib [0..]

--2-- linear version of fib list --magic zipWith version
fibs2 =  0 : 1 : zipWith (+) fibs2 (tail fibs2)

--3--Stream type for only infinite lists
data Stream a = Cons a (Stream a)

--convert stream to list 
streamToList :: Stream a -> [a]
streamToList (Cons x s) = x : streamToList s

--instance of show using list function
instance Show a =>  Show (Stream a) where
  show = show . take 20 . streamToList

--4--tools for working with Streams

--generate a stream containing infintiely many copies of the given element
streamRepeat :: a -> Stream a
streamRepeat x = Cons x (streamRepeat x)

--applies a function to every element of a stream
streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Cons x s) = Cons (f x) (streamMap f s)

--generates a stream from a seed and unfolding rule
streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f x = Cons x (streamFromSeed f (f x))

--5--make some streams!

--natural numbers
nats :: Stream Integer
nats = streamFromSeed (+ 1) 0

--interleaving streams
interleaveStreams (Cons x s) l = Cons x (interleaveStreams l s)

--interleaving elements of two streams
--I gave up! :'(
---found this on http://stackoverflow.com/questions/37174707/haskell-cis194-spring-13-homework-6-exercise-5
ruler :: Stream Integer
ruler = interleaveStreams (streamRepeat 0) (streamMap (+1) ruler)
--we need something like...
--       interleave (streamRepeat 0) (interleave (streamRepeat 1) (interleave (streamRepeat 2) ...))

--6-- fibonacci numbers via generating functions
--same as 0 + 1x+ 0x^2 + 0x^3 + ....
x :: Stream Integer
x = Cons 0 (Cons 1 (streamRepeat 0))

--instance of Num for Stream Integer
instance Num (Stream Integer) where
  fromInteger n  = Cons n (streamRepeat 0)
  negate (Cons x s) = Cons (0 - x) (negate s)
  (+) (Cons x s) (Cons y t) = Cons (x + y) (s + t)
  (*) (Cons x s) b@(Cons y t) =  Cons (x * y) ((streamMap (\n -> x * n) t) + (s * b)) --need xy + xt + sb

--instance of Fractional (for division)  
instance Fractional (Stream Integer) where
   (/) (Cons x s) b@(Cons y t) = q
         where
            q = Cons ( x `div` y) (streamMap (\n -> 1 `div` y * n) (s - (q * t)))

--now fibonacci is just x / (1 - x - (x * x))
fibs3 :: Stream Integer
fibs3 = x / (1 - x - (x * x))

--7-- Fibonacci numbers via matrices
--data type for 2 X 2 matrices of Integers

data Matrix = M Integer Integer Integer Integer
                deriving Show

{- so we have
                   [e f ]
                   [g h]
            [a b]
            [c d]
-}
instance Num Matrix where
  (*) (M a b c d) (M e f g h)  = M ((a * e) + (b * g))
                                                     ((a * f) + (b * h))
                                                      ((c * e) + (d * g))
                                                     ((c * f) + (d * h))

--get the nth fibonnaci number
fib4 :: Integer -> Integer
fib4 n = getN (ff^n)
    where
         ff = M 1 1 1 0
         getN (M _ n _ _) = n
