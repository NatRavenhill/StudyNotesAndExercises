import Data.List

--mid chapter exercises--
--1- a function that takes a List a and generates a [a]
data List a = Cons a (List a) | Nil
            deriving (Show)

toList :: List a -> [a]
toList Nil = []
toList (Cons x xs) = x : toList xs

--2- Define a tree type that has only one constructor, like our Java example. Instead of the Empty constructor, 
-- use the Maybe type to refer to a node's children.
data MayTree a = Maybe (a (Maybe (MayTree a)) (Maybe (MayTree a)))

--End of chapter exercises--
--1 and 2- --compute the number of elements in a list
length' :: (Num a) => [b] -> a
length' [] = 0
length' (x:xs) = 1 + length' xs 

--3- --compute the mean of a list
--sum returns a Num
--length returns an Int
--fromIntegral converts an integral to a num
mean xs = (sum  xs) / fromIntegral (length xs)

--4- --turn a list into a palindrome
palindrome [] = []
palindrome (x:xs) = (x : palindrome xs) ++ [x]

--5- --determine if input list is palindrome
isPalindrome :: (Eq a) =>  [a] -> Bool
isPalindrome [] = True
isPalindrome [x] = True
isPalindrome (x:xs)
     | f (x:xs) /= x = False
     | otherwise = isPalindrome (fst (splitInput xs))
         where 
            splitInput xs = splitAt ((length xs) - 1) xs
            f = head . snd . splitInput

--6- --sort a list of lists based on the length of each sublist.
sortListsByLength xs = sortBy (\x y -> compare (length x) (length y)) xs

--7-- join a list of lists together using a separator value
intersperse' :: a -> [[a]] -> [a]
intersperse' a [] = []
intersperse' a (l:ls) = case ls of
                        (x:xs) -> l ++ [a] ++ (intersperse' a ls)
                        otherwise -> l

--8-- binary tree height
data Tree a = Node a (Tree a) (Tree a)
            | Empty
            deriving (Show)

exampleTree = Node 5 (Node 3 (Node 4 Empty Empty) Empty) Empty

height :: Tree a -> Int
height Empty = 0
height (Node x l r)  = 1 + (max (height l) (height r))

--9-- direction data type
type Point = (Int,Int)
data Direction = TurnsLeft | TurnsRight | StraightLine
                 deriving Show 
  




         

        
             


