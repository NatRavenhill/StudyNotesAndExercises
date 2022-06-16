{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Calc where

import ExprT
import Parser
import StackVM
import qualified Data.Map as M

--1--evaluator for ExprT
eval :: ExprT -> Integer
eval (Lit n) = n
eval (ExprT.Add x y) = (eval x) + (eval y)
eval (ExprT.Mul x y) = (eval x) * (eval y)

--2-- evaluate arithmetic expressions given a string, returning nothing if not well formed and Just result otherwise
evalStr :: String -> Maybe Integer
evalStr s = parseExp (\n -> eval (Lit n)) (\m n -> m + n) (\m n -> m * n) s

--3-- type class called Expr with three methods which parallel constructors of ExprT
class Expr a where 
    lit :: Integer -> a
    add :: a -> a -> a
    mul :: a -> a -> a

--instance for ExprT
instance Expr ExprT where
  lit n = Lit n
  add x y = ExprT.Add x y
  mul x y = ExprT.Mul x y
  
--constrains argument type to ExprT
reify :: ExprT -> ExprT
reify = id

--4--instances for common types
instance Expr Integer where
  lit n = n
  add x y = x + y
  mul x y = x * y

--all positive integers are true, add is or and mul is and
instance Expr Bool where
   lit n = (n > 0)
   add x y = x || y
   mul x y = x && y

newtype MinMax = MinMax Integer deriving (Eq, Show)

instance Expr MinMax where
  lit n = MinMax n
  add (MinMax x) (MinMax y) = MinMax (max x y)
  mul (MinMax x) (MinMax y) = MinMax (min x y)

newtype Mod7 = Mod7 Integer deriving (Eq, Show)

instance Expr Mod7 where
  lit n = Mod7 (n `mod` 7)
  add (Mod7 x) (Mod7 y) = Mod7 ((x + y) `mod` 7)
  mul (Mod7 x) (Mod7 y) = Mod7 ((x * y) `mod` 7)
  
--testing function
testExp :: Expr a => Maybe a
testExp = parseExp lit add mul "(3 * -4) + 5"

--5--create a compiler for arithmetic expressions using the stack machine
instance Expr Program where
  lit n = [PushI n]
  add xs ys = xs ++ ys ++ [StackVM.Add]
  mul xs ys = xs ++ ys ++ [StackVM.Mul]

compile :: String -> Maybe Program
compile s =  parseExp lit add mul s

--6--
--create a new class HasVars where types of this class have some notio of named variables
class HasVars a where
  var :: String -> a

 --create a datatype VarExprT which is the same as ExprT but with an extra constructor for variables
data VarExprT = LIT Integer
           | ADD VarExprT VarExprT
           | MUL VarExprT VarExprT
           | VAR String
  deriving (Show, Eq)

--make it an instance of Expr and HasVars
instance Expr VarExprT where
   lit n = LIT n
   add x y = ADD x y
   mul x y = MUL x y

instance HasVars VarExprT where
   var s = VAR s

--interpret expressions containing variables by implementing these instances
--variables can be interpreted as functions from a mapping of variables to Integer values to (possibly) Integer value
instance HasVars (M.Map String Integer -> Maybe Integer) where
  var s = M.lookup s

--function for adding
testAdd :: (M.Map String Integer -> Maybe Integer) -> (M.Map String Integer -> Maybe Integer) -> M.Map String Integer -> Maybe Integer
testAdd f g m = case (f m) of
                       Nothing ->  Nothing
                       Just x  -> case (g m) of
                                         Nothing -> Nothing
                                         Just y -> Just (x + y)

--fiunction for multiplication
testMul :: (M.Map String Integer -> Maybe Integer) -> (M.Map String Integer -> Maybe Integer) -> M.Map String Integer -> Maybe Integer
testMul f g m = case (f m) of
                       Nothing ->  Nothing
                       Just x  -> case (g m) of
                                         Nothing -> Nothing
                                         Just y -> Just (x * y)

instance Expr (M.Map String Integer -> Maybe Integer) where
  lit n = (\m -> Just n)
  add x y = (\m -> testAdd x y m) 
  mul x y =  (\m -> testMul x y m) 

--function for test ing
withVars :: [(String, Integer)]
               -> (M.Map String Integer -> Maybe Integer)
               -> Maybe Integer
withVars vs exp = exp $ M.fromList vs
