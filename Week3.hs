module Week3 where

type Identifier = String

{-

Exercise (10 points). We have added a ternary operator like the Java or C
conditional expression e1?e2:e3, here written Op Cond [e1, e2, e3].
But we didn't implement this in our interpreter. Add a suitable definition (where?).

-}


data OpName = Cond -- We are adding this new ternary operator for you to implement.
            | Add
            | Sub
            | Mul
            | Div
            | Neg
            | Eq
            | Leq
            | Less
            | Geq
            | Greater
            | And
            | Or
            | Not
            deriving (Eq, Show)

data Expr = Constant Int
          | Var Identifier
          | Op OpName [Expr]
          deriving (Eq, Show)

data Program = Identifier := Expr
             | IfThenElse Expr Program Program
--           | IfThen Expr Program              -- we are removing this
             | While Expr Program
             | Block [Program]
             deriving (Eq, Show)

{-

Prelude to the next exercise. IfThen is not really necessary:

-}

skip :: Program
skip = Block []    -- does nothing.

ifThen :: Expr -> Program -> Program
ifThen e p = IfThenElse e p skip

{-

Exercise (10 points). Similar to the above, define a for-loop program
constructor by reduction to while-loops.

-}

for :: Program -> Expr -> Program -> Program -> Program
for init test update body = Block [ init, While test (Block[body,update])]

{-

    In concrete syntax

            for (init; test; update)
                body;

    should be equivalent to

            init;
            while (test)
              {
                body;
                update;
              }
-}

{-

Example. A program for calculating factorial, with the
convention that the input is x and the output is y.


  {
   y := 1;
   for (n := x; n > 0; n := n - 1)
     y := y * n;
  }

Exercise (10 points). Write this program in abstract syntax (using
your "for" function):

-}

factorial :: Program
factorial = 
	Block
		[
			"y" := Constant 1,
			for("n" := Var "x") (Op Greater [Var "n",Constant 0]) ("n" := Op Sub [Var "n", Constant 1]) ("y" := Op Mul [Var "y", Var "n"])
			--init test update body
		]
		
		
		
code :: Bool -> Int
code False = 0
code True  = 1

decode :: Int -> Bool
decode 0 = False
decode _ = True

opeval :: OpName -> [Int] -> Int
opeval Cond    [x,y,z] = if decode x then y else z
opeval Add     [x, y] = x + y
opeval Sub     [x, y] = x - y
opeval Mul     [x, y] = x * y
opeval Div     [x, y] = x `div` y
opeval Neg     [x]    = - x
opeval Eq      [x, y] = code(x == y)
opeval Leq     [x, y] = code(x <= y)
opeval Less    [x, y] = code(x < y)
opeval Geq     [x, y] = code(x >= y)
opeval Greater [x, y] = code(x > y)
opeval And     [x, y] = code(decode x && decode y)
opeval Or      [x, y] = code(decode x || decode y)
opeval Not     [x]    = code(not(decode x))
opeval op      xs     = error ("Tried to apply " ++ (show op) ++ " to " ++ show xs)

type Memory = Identifier -> Int

mExample :: Memory
mExample "x" = 3
mExample "y" = 4
mExample "z" = 5
mExample _   = undefined

eval :: Memory -> Expr ->  Int
eval m (Constant x) = x
eval m (Var i)      = m i
eval m (Op o es)    = opeval o [eval m e | e <- es]

write :: Identifier -> Int -> Memory -> Memory
write i x m = m'
 where
   m' :: Memory
   m' j
     | i == j    = x
     | otherwise = m j

run :: Program -> Memory -> Memory

run (i := e) m = write i (eval m e) m

run (IfThenElse e p q) m
    | decode(eval m e) = run p m
    | otherwise        = run q m

-- run (IfThen e p) m  -- removed because we removed IfThen from Program.
--    | decode(eval m e)  = run p m
--    | otherwise         = m

run (While e p) m
    | decode(eval m e) = m''
    | otherwise        = m
    where
      m'  = run p m
      m'' = run (While e p) m'

run (Block []) m = m

run (Block (p : ps)) m = m''
    where
      m'  = run p m
      m'' = run (Block ps) m'

runxy :: Program -> Int -> Int
runxy p x = m' "y"
 where
   m :: Memory
   m "x" = x
   m i   = error ("Non-existent memory location " ++ i)

   m' :: Memory
   m' = run p m

{-

Example, continued from the above (you can use this to test your solutions):

-}

fact :: Int -> Int
fact = runxy factorial

{-

There is another way to represent memory:

-}

type Memory' = [(Identifier,Int)]

{-

Exercise (15 points). Fill the following undefined's for this new
definition of memory (there are 4 undefined's)

-}

mExample' :: Memory'
mExample' = [("x", 3), ("y", 4), ("z", 5)]

read' :: Identifier -> Memory' -> Int
read' z [] = error ("Identifier not in list")
read' z ((x,y):xs) = if x == z then y else read' z xs

eval' :: Memory' -> Expr ->  Int
eval' m (Constant x) = x
eval' m (Var i)      = read' i m
eval' m (Op o es)    = opeval o [eval' m e | e <- es]

write' :: Identifier -> Int -> Memory' -> Memory'
write' i x m = [(i,x)]

run' :: Program -> Memory' -> Memory'

run' (i := e) m = write' i (eval' m e) m

run' (IfThenElse e p q) m
    | decode(eval' m e) = run' p m
    | otherwise        = run' q m

run' (While e p) m
    | decode(eval' m e) = m''
    | otherwise        = m
    where
      m'  = run' p m
      m'' = run' (While e p) m'

run' (Block []) m = m

run' (Block (p : ps)) m = m''
    where
      m'  = run' p m
      m'' = run' (Block ps) m'

runxy' :: Program -> Int -> Int
runxy' p x = undefined

{-

Example, continued from the above:

-}

fact' :: Int -> Int
fact' = runxy' factorial
