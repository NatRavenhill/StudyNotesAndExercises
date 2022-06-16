{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}

import Data.Monoid
import Sized
import Scrabble
import Buffer
import Editor

data JoinList m a = Empty | Single m a | Append m (JoinList m a) (JoinList m a)
    deriving (Eq, Show)

ex1 :: JoinList (Product Integer) String
ex1 = Append (Product 210) (Append (Product 30) (Single (Product 5) "y") (Append  (Product 6) (Single (Product 2) "e") (Single (Product 3) "a"))) (Single (Product 7) "h")

ex2 =  Append  (Product 6) (Single (Product 2) "e") (Single (Product 3) "a")
ex3 = Single (Product 4) "t"

-- tag gets the annotation at the root of a JoinList
tag :: Monoid m => JoinList m a -> m
tag Empty = mempty
tag (Single m _) = m
tag (Append m _ _) = m

-- append function for JoinLists
(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
(+++) Empty l2 = l2
(+++) (Single m a) l2 =  Append (m <> tag l2) (Single m a) l2
(+++) (Append m l1 l2) l3 = Append (m <> tag l3) (Append m l1 l2) l3


jlToList :: JoinList m a -> [a]
jlToList Empty = []
jlToList (Single _ a) = [a]
jlToList (Append _ l1 l2) = jlToList l1 ++ jlToList l2

(!!?) :: [a] -> Int -> Maybe a
[] !!? _ = Nothing
_ !!? i | i < 0 = Nothing
(x:xs) !!? 0 = Just x
(x:xs) !!? i = xs !!? (i-1)

ex4 :: JoinList Size [Char]
ex4 = Single (Size 1) "m"

-- indexJ finds the JoinList element at the specified index
indexJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
indexJ x Empty = Nothing
indexJ x (Single b a)
 | x == 0 = Just a
 | otherwise = Nothing
indexJ x (Append b l1 l2)
    | x > pos = Nothing
    | x < innerPos1 = indexJ x l1
    | otherwise = indexJ (x-1) l2
    where
        pos = getSize (size b) -- 2
        innerPos1 = getSize (size (tag l1))

--dropJ drops the first n elements from a JoinList
dropJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
dropJ n Empty = Empty
dropJ 0 l = l
dropJ n (Single _ _) = Empty
dropJ n (Append b l1 l2)
    | n <= l1Size =  dropJ n l1 +++ l2
    | otherwise = Empty +++ dropJ (n - l1Size) l2
        where
            l1Size = getSize $ size $ tag l1

-- takeJ takes the first n values from a JoinList and drops the rest
takeJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
takeJ 0 _ = Empty
takeJ n Empty = Empty
takeJ n  (Single b a) = Single b a
takeJ n (Append b l1 l2)
    | n <= l1Size = takeJ n l1
    | otherwise = takeJ l1Size l1 +++ takeJ (l1Size - n) l2
         where
            l1Size = getSize $ size $ tag l1


ex5 :: JoinList Size [Char]
ex5 = Append (Size 2)
      (Single (Size 1) "hi")
      (Single (Size 1) "bye")

scoreLine :: String -> JoinList Score String
scoreLine s = Single (scoreString s) s

instance Monoid m => Semigroup (JoinList m a) where
    (<>) = (+++)

instance Monoid m => Monoid (JoinList m a) where
  mempty  = Empty

instance Buffer (JoinList (Score, Size) String) where
  toString = unlines . jlToList
  fromString s = listToJL inputs
    where
        inputs = lines s
        listToJL [] = Empty
        listToJL [x] = Single (scoreString x, Size (length x)) x
        listToJL (x:xs) = listToJL [x] +++ listToJL  xs
  line n jl = jlToList jl !!? n
  replaceLine n s jl 
    | n >= numLines jl = jl
    | otherwise = takeJ n jl <> fromString s <> dropJ (n + 1) jl
  numLines = length . jlToList
  value = getScore . fst . tag

ex6 :: JoinList (Score, Size) [Char]
ex6 = Append (Score 18, Size 2) (Single (Score 4, Size 1) "test") (Single (Score 14, Size 1) "queen")

main :: IO ()
main = do
    let buffer = ex6
    runEditor editor buffer