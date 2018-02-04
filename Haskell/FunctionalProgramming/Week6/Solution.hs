module Solution where


{-

You must use monadic parsing, with the ParseLib library, in the style
of the lecture file MonadicParserExamples.hs.

-}


import AbstractSyntax
import ParseLib
import Data.Char
import Control.Monad

{- The BNF grammar for Program
   
    Program ::= Variable := Expr; 
              | If (Expr) Program 
              | If (Expr) Program else Program | 
              | while (Expr) Program
              | { [Program] }

You will need to factorize the If syntax, to get one clause only, as
we did in the lectures (see an example of factorization in
MonadicParserExamples.hs).

-}

{-

In order to solve this exercise, use the ideas from 
http://www.cs.bham.ac.uk/~mhe/functional-programs-2013/MonadicParserExamples.hs

The ideas include factorization, the possible use of chainR1 (if you
wish), what keywords below means, etc.

-}

{-
    
Exercise (20 + 20 points). Complete the following definition:

20 points if you implement +,-,*,/,> with the right precedence.

20 more points if you implement all the operations with the right
precedence (given in lab5).

-}
{-Program ::= Variable := Expr; 
          | If (Expr) Program 
          | If (Expr) Program else Program | 
          | while (Expr) Program
          | { [Program] }
Or ::= And | (And | And) 
And ::= Eq |Eq & Eq 
Eq ::= Expr | Expr == Expr
Expr ::= Arith | Arith > Expr | Arith < Expr | Arith >= Expr | Arith <= Expr
Arith ::= Term | Term AddOp Arith
Term ::= Factor | Not MulOp Not
Not ::= Factor | NotOp Factor
Factor := Constant | Variable | (Expr)
AddOp := + | -
MulOp := * | /
NotOp := ! -}

expr, orexpr, andexpr, eq, arith, term, notexpr, factor, constant, variable  :: Parser Expr
moreop, addop, mulop :: Parser ([Expr] -> Expr)

expr = andexpr `chainR1` (parop "|" Or)
andexpr = orexpr `chainR1` (parop "&" And)
orexpr = eq `chainR1` (parop "==" Eq)
eq = arith `chainR1` moreop
arith = term `chainR1` addop
term = notexpr `chainR1` mulop
notexpr = do {f <- symb "!"; a <- notexpr; return (Op Not [a])} +++ factor
constant = do {n <- int ; return(Constant n)} 
variable = do {s <- ident keywords; return(Var s)} 
factor = do { symb "("; e <- expr; symb ")"; return e }
     +++ constant 
     +++ variable
	 	 
moreop = parop' inequalities
addop = parop "+" Add +++ parop "-" Sub
mulop = parop "*" Mul +++ parop "/" Div +++ parop "%" Mod
  
inequalities = [("<=" , Leq) , ("<" , Less) , (">=" , Geq) , (">", Greater)] 
 
chainR1         :: Parser a -> Parser ([a] -> a) -> Parser a
p `chainR1` op   = do {a <- p; rest a}
                    where
                      rest a = do {f <- op; b <- p; r <- rest b; return(f[a, r])}
                               +++ return a

{-

You may find the following helper functions useful. Use them if you
wish (you are not obliged to):

-}

-- Parse an n-ary operator:
parop :: String -> OpName -> Parser ([Expr] -> Expr)
parop s op = do { symb s; return(Op op) } 

-- Parse one of a list of n-ary operators (the first that
-- succeeds). This is useful if you include in your list all
-- operations with a given precedence:
parop' :: [(String, OpName)] -> Parser ([Expr] -> Expr)
parop' [] = mzero
parop' ((s, op) : xs)  = parop s op +++ parop' xs
 
{-

You probably want to complete the following definition, if you use the
same approach as in MonadicParserExamples.hs. If not, just leave it is
as it is:

-}

keywords = ["if", "else","while"]

{-

Exercise (40 points). Complete the following definition. To get any
marks here, you will need to have implemented correctly at least
+,-,*,/,> in the previous exercise (because you cannot have a correct
program parser if you don't have a correct expression parser, as
programs contain subexpressions).

Hint. To get started, check the incomplete definition given at
MonadicParserExamples.hs

-}

program :: Parser Program                      
program = do { i <- ident keywords; symb ":="; e <- expr; symb ";"; return(i := e)}
	  +++ do { symb "if"; symb "("; e <- expr; symb ")"; p <- program; symb "else" ; q <- program; return(IfThenElse e p q)}
      +++ do { symb "if"; symb "("; e <- expr; symb ")"; p <- program; return(IfThen e p)}
	  +++ do { symb "while"; symb "("; e <- expr; symb ")"; p <- program; return (While e p)}
	  +++ do { symb "{"; p <- programList; symb "}"; return (Block p)}


{-

Hint: you can use the following as a helper function to parse blocks
in the above definition if you wish (you are not obliged to):

-}

programList :: Parser [Program]
programList = many program

-- You can run the parsers as follows for testing:

pars :: String -> Expr
pars = apply' expr

parsProg :: String -> Program
parsProg = apply' program
