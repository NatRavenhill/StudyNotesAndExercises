{-

Exercise (40 points). Fill in the undefined pieces of code below. You
should return mzero when division by zero occurs, either in the
operation / or %. Notice that probably you are not going to return
mzero explicitly, but instead you are likely to prefer to use the
"guard" function (as in the handout twomonads.hs).

-}


module Interpreter where

import Control.Monad
import AbstractSyntax
                     
code :: Bool -> Int
code False = 0
code True  = 1

decode :: Int -> Bool
decode 0 = False  
decode _ = True

opeval :: MonadPlus m => OpName -> [Int] -> m Int
opeval Add [x, y] =  return (x + y)		
opeval Sub [x, y] = return (x - y)
opeval Mul [x, y] = return (x * y)
opeval Div [x, y] = do { guard(y /= 0); return (x `div` y)}
opeval Mod [x,y] =  do { guard(y /= 0); return (x `mod` y)}
opeval Eq  [x, y] = return (code(x == y))
opeval Leq  [x, y] = return (code(x <= y))
opeval Less    [x, y] = return (code(x < y))
opeval Geq     [x, y] = return (code(x >= y))
opeval Greater [x, y] = return (code(x > y))
opeval And     [x, y] = return (code(decode x && decode y))
opeval Or      [x, y] = return (code(decode x || decode y))
opeval Not     [x]    = return (code(not(decode x)))
opeval op      xs     = error ("Runtime error: Tried to apply " ++ (show op) ++ " to " ++ show xs)
					

type Memory m = Identifier -> m Int
           
memExample :: MonadPlus m => Memory m
memExample "x" = return 3
memExample "y" = return 4
memExample "z" = return 5
memExample _ = mzero  -- Gives an (abstract) "error" for undefined variables.

-- Hint: Use mapM :: Monad m => (a -> m b) -> [a] -> m [b] 
eval :: MonadPlus m => Memory m -> Expr ->  m Int
eval mem (Constant x) = return x
eval mem (Var i)      = mem i
eval mem (Op o es)    = do {x <- mapM (eval mem) es; opeval o x}

write :: MonadPlus m => Identifier -> Int -> Memory m -> Memory m
write i x mem = mem'
    where mem' j = if i == j 
						then return x 
						else mem j
                      
run :: MonadPlus m => Program -> Memory m -> m(Memory m)
run (i := e) mem = do {x <- eval mem e ; return (write i x mem)}

run (IfThenElse e p q) mem =
    do x <- eval mem e
       if decode x 
          then run p mem
          else run q mem
		  
               
run (IfThen e p) mem  = 
	do x <- eval mem e
	   if decode x
		  then run p mem
		  else  return mem

		  
{-run (While e p) mem  = 
	do x <- eval mem e
	   if decode x 
		   then do x <- While e p
				run x mem''
		   else return mem'
	where 
		mem' = run p mem
		mem'' = run (While e p) mem'-} --fix me


run (Block []) mem = return mem

{-run (Block (p : ps)) mem =  mem''
    where mem'  = run p mem
		  mem'' = run (Block ps) mem'-} --fix me

		


runxy :: MonadPlus m => Program -> Int -> m Int
runxy p x = do mem' <- run p mem
               mem' "y"
 where
   mem "x" = return x
   mem _   = mzero
