module Week4 where

--Exercise 1--

fun1 :: [Integer] -> Integer
fun1 = product . map (\x -> x - 2) . filter (even)

fun2 :: Integer -> Integer
fun2 1 = 0
fun2 n
  | even n =  n +  fun2 (n `div` 2)
  | otherwise = fun2 (3 * n + 1)

fun2' :: Integer -> Integer
fun2' = sum . filter (even) . takeWhile (/= 1) . iterate (\n -> if even n then n `div` 2 else 3*n+1)

--Exercise 2--
data Tree a = Leaf | Node Integer (Tree a) a (Tree a)
          deriving (Show, Eq)

--create a balanced Tree containing heights of the nodes
--balanced tree
balancedInsert :: a -> Tree a -> Tree a
balancedInsert a Leaf = Node 0 Leaf a Leaf
balancedInsert a t@(Node n l x r)
   | height r < height l = Node n l x (balancedInsert a r)
   | otherwise = Node n (balancedInsert a l) x r
   
--height function
height :: Tree a -> Integer
height Leaf = 0
height (Node _ l x r) = 1 + max (height l) (height r)

newHeight :: Tree a -> Tree a
newHeight Leaf = Leaf
newHeight t@(Node _ l x r) = Node ((height t)-1) (newHeight l) x (newHeight r)

--insert from list
listInsert :: (Ord a) => [a] -> Tree a
listInsert l = newHeight $ foldr balancedInsert Leaf l

--Exercise 3--
--returns True if and only if there are an odd number of True values contained in the input list
xor = odd . length . filter (\x -> x == True)

--implement map as a fold
map' :: (a -> b) -> [a] -> [b]
map' f l = foldr (\x xs -> f x : xs) [] l

--implement filter as a fold
filter' :: (a -> Bool) -> [a] -> [a]
filter' p l = foldr (\x xs -> if p x then x : xs else xs) [] l 

--implement foldl using foldr
myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl f base xs = foldr (flip f) base (reverse xs)
     
