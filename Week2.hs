module Week2 where

data Expr = Constant Int
          | Add Expr Expr
          | Sub Expr Expr
          | Mul Expr Expr
          | Div Expr Expr
          deriving (Eq, Show)

-- "Eq" is for telling Haskell to derive code to compare expressions
-- for syntactical equality "==". "Show" is for telling Haskell to
-- derive code to convert Exprs to Strings using the show
-- function. But We prefer to use our own show function, that will use
-- a more conventional representation:

exprShow :: Expr -> String
exprShow (Constant n) = show n
exprShow (Add e1 e2)  = "(" ++ exprShow e1 ++ "+" ++ exprShow e2 ++ ")"
exprShow (Sub e1 e2)  = "(" ++ exprShow e1 ++ "-" ++ exprShow e2 ++ ")"
exprShow (Mul e1 e2)  = "(" ++ exprShow e1 ++ "*" ++ exprShow e2 ++ ")"
exprShow (Div e1 e2)  = "(" ++ exprShow e1 ++ "/" ++ exprShow e2 ++ ")"

-- 2 * 4 + 5
eExample :: Expr
eExample = Add (Mul (Constant 2) (Constant 4)) (Constant 5)

-- Using the following function, the above expression evaluates to 13:
eval :: Expr -> Int
eval (Constant n) = n
eval (Add e1 e2)  = eval e1 + eval e2
eval (Sub e1 e2)  = eval e1 - eval e2
eval (Mul e1 e2)  = eval e1 * eval e2
eval (Div e1 e2)  = eval e1 `div` eval e2
          
-- Exercise (10 points). Complete the following definition so that
-- when division by zero occurs within a subexpression, the result of
-- the evaluation of the expression is Nothing.

eval' :: Expr -> Maybe Int
eval' (Constant n) =  Just n
eval' (Add e1 e2)  = Just (eval e1 + eval e2)
eval' (Sub e1 e2)  = Just (eval e1 - eval e2)
eval' (Mul e1 e2)  = Just (eval e1 * eval e2)
eval' (Div e1 e2) = if e1 == Constant 0 || e2 == Constant 0 then Nothing else Just (eval e1 `div` eval e2)
				

-- We can define our own type of lists, so that we write 
-- Cons 1 (Cons 2 (Cons 3 Nil)) instead of 1 : 2 : 3 : [] or
-- equivalently [1,2,3].

data List a = Nil | Cons a (List a)
     deriving (Eq, Show) 

-- Exercise (10 points). Show that this type is isomorphic to [a], by
-- defining the following functions.

toNormal :: List a -> [a]
toNormal Nil = []
toNormal (Cons x (Nil)) = x : toNormal Nil
toNormal (Cons x xs) = x : toNormal xs 

fromNormal :: [a] -> List a
fromNormal [] = Nil
fromNormal (x:xs) = Cons x (fromNormal(xs))

-- We should have toNormal(fromNormal xs) = xs
-- and            fromNormal(toNormal xs) = xs.
-- (This is what isomorphism (defined in Foundations 1) means.)

-- Exercise (10 points). Define a "listShow" function for List a, like we
-- did for Expr. For example, the list Cons 3 (Cons 4 (Cons 5 Nil))
-- should be shown as "{3; 4; 5}". 

listShow :: Show a => List a -> String     
listShow Nil = "{}"
listShow (Cons n xs) = "{" ++ conShow(Cons n xs) ++ "}"
 where
  conShow :: Show a => List a -> String
  conShow (Cons n Nil) = (show n)
  conShow(Cons n xs) = (show n) ++ ";" ++ conShow(xs) 
  
-- Exercise (10 points). Define a listMap function satisfying
--
-- listMap f (Cons x0 (Cons x1 (Cons x2 ... Nil)))
--       = (Cons (f x0) (Cons (f x1) (Cons (f x2) ... Nil)))
--
-- E.g. listMap odd (Cons 3 (Cons 4 (Cons 5 Nil))) = Cons True (Cons False (Cons True Nil))

listMap :: (a -> b) -> List a -> List b
listMap f Nil = Nil
listMap f (Cons x xs) = (Cons (f x)) (listMap f xs)

-- Exercise (10 points). Define a filter function that keeps all the
-- elements satisfying a given property and discards the other ones.
--
-- E.g. listFilter odd (Cons 3 (Cons 4 (Cons 5 Nil))) = Cons 3 (Cons 5 Nil)

listFilter :: (a -> Bool) -> List a -> List a
listFilter f Nil = Nil
listFilter f (Cons x xs) = (if f x == True then (Cons x) (listFilter f xs) else  listFilter f xs)

-- Now consider trees.

data Tree  a = Empty | Fork  a (Tree a) (Tree a) deriving (Show, Eq)
data Tree' a = Leaf a | Branch (Tree' a) (Tree' a) deriving (Show, Eq)

-- Exercise (10 points). Write two functions

flatten :: Tree a -> [a]
flatten Empty = []
flatten (Fork x l r) = [x] ++ flatten l ++ flatten r

unflatten :: [a] -> Tree a
unflatten [] = Empty
unflatten (x:xs) = Fork x (unflatten(xs)) Empty

-- that satisfy the property flatten(unflatten xs) = xs for any given xs.

-- Exercise (10 points). Write two functions

flatten' :: Tree' a -> [a]
flatten' (Leaf x) = [x]
flatten' (Branch l r) = flatten' r ++ flatten' l

unflatten' :: [a] -> Tree' a
unflatten' [] = error "Can't unflatten []"
unflatten'[x] = Leaf x
unflatten'(x:xs) = Branch (unflatten' xs) (Leaf x)

-- such that flatten'(unflatten' xs) = xs for
-- any given *non-empty* list xs.
--
-- Notice unflatten' [] is problematic. Make it undefined or give an
-- error using the built-in function "error".
