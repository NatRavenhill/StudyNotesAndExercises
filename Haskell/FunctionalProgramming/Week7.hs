module Week7 where
import Data.Monoid

{-
The identity monad. Does nothing!

We just save an element in a box.
-}

data Id a = Box a deriving Show

instance Monad Id where
  return a = Box a
  (Box a) >>= f = f a


-- There is no need to use a monad, but we can:
mfib :: Monad m => Integer -> m Integer
mfib n =
  if n == 0 || n == 1
         then return n
         else do x <- mfib(n-1)
                 y <- mfib(n-2)
                 return(x + y)

-- Exercise (10 points): Implement the factorial function similarly
-- (using recursion) Hint: 0! = 1
fact :: Monad m => Integer -> m Integer
{-fact n =
	if n == 0
		then return 1
		else return (n * fact'(n-1))
			where fact' n = if n==0 then 1 else  n * fact'(n-1)-}
fact n =
	if n == 0
		then return 1
		else do {x <- return n;
				y <- fact(n-1);
				return(x*y)}

-- Without using the monad, mfib amounts to:

fib :: Integer -> Integer
fib n =
  if n == 0 || n == 1
         then n
         else fib(n-1) + fib(n-2)

{-

The above function computes in exponential time.
Let's make it faster.

-}

fib' :: Integer -> Integer
fib' n = f n 0 1
  where
    f 0 a b = a
    f n a b = f(n-1) b (a+b)

{- This is linear time because there is one recursive call only. -}

{-

Let's trace the execution of fib by "printing" while we compute.  But
without the IO monad.

Let's work with a simplified writer monad, written by ourselves.

Now we put the thing we want in a box, but we also store a String. We
call the box "Writer" (rather than "Box" above).

-}

data Writer a = Writer a String deriving Show

instance Monad Writer where
  return a = Writer a []
  (Writer a xs) >>= f = Writer b (xs ++ ys)
    where
      Writer b ys = f a

tell :: String -> Writer ()
tell xs = Writer () xs

wfib :: Integer -> Writer Integer
wfib n = do
  tell (" call with " ++ show n ++ ", ")
  if n == 0 || n == 1
         then return n
         else do x <- wfib(n-1)
                 y <- wfib(n-2)
                 return(x + y)

-- Equivalently:

wfib' :: Integer -> Writer Integer
wfib' n =
  tell (" call with " ++ show n ++ ", ") >>
  (if n == 0 || n == 1
         then return n
         else wfib(n-1) >>= (\x -> wfib(n-2) >>= (\y -> return(x + y))))

-- Exercise (unassessed and not checked (so make your own tests)):
-- Implement the factorial function similarly
wfact :: Integer -> Writer Integer
wfact n = 
	tell (" call with " ++ show n ++ ", ") >>
	(if n == 0
		then return 1
				else do {x <- return n;
				y <- wfact(n-1);
				return(x*y)})


{-

We can use a similar monad to keep track of the number of recursive
calls.

-}

data Counter a = Counter a Integer deriving (Eq, Show)

instance Monad Counter where
  return a = Counter a 0
  (Counter a x) >>= f = Counter b (x + y)
    where
      Counter b y = f a

count :: Counter ()
count = Counter () 1

cfib :: Integer -> Counter Integer
cfib n = do
  count
  if n == 0 || n == 1
         then return n
         else do x <- cfib(n-1)
                 y <- cfib(n-2)
                 return(x + y)

-- Exercise (10 points): Implement a version of the factorial function
-- that counts the number of calls.
cfact :: Integer -> Counter Integer
cfact n = do
	count
	if n == 0
		then return 1
		else do {x <- return n;
				y <- cfact(n-1);
				return(x*y)}

{- We now do the state monad, not done in class. -}

data State s a = State(s -> (a,s))

runState :: State s a -> s -> (a, s)
runState (State f) = f

instance Monad (State s) where
  return x = State(\s -> (x,s))
  (State h) >>= f = State(\s -> let (a, s') = h s
                                    (State g) = f a
                                in  g s')

get :: State s s
get = State(\s -> (s,s))

put :: s -> State s ()
put s = State (\_ -> ((), s))

modify :: (s -> s) -> State s ()
modify f = State(\s -> ((), f s))

{-

In the following example, the state consists of one Integer, which we
use to count recursive calls. We modify the state by applying the
successor function. The other Integer is not the state, but rather the
result of the function.

-}

sfibHelper :: Integer -> State Integer Integer
sfibHelper n = do
        modify succ
        if n == 0 || n == 1
          then return n
          else do x <- sfibHelper(n-1)
                  y <- sfibHelper(n-2)
                  return(x + y)


{- Returns the value of the fib function and the number of recursion unfoldings used: -}

sfib :: Integer -> (Integer , Integer)
sfib n = runState (sfibHelper n) 0

-- Exercise (10 points). Instead of counting the number of recursive
-- calls, record all arguments used in recursive calls in a list. This
-- is a simple modification to sfibHelper.

sfib' :: Integer -> State [Integer] Integer 
sfib' n = do
        let f l = n : l in
		modify f
        if n == 0 || n == 1
          then return n
          else do x <- sfib'(n-1)
                  y <- sfib'(n-2)
                  return(x + y)			
		

-- This can be done in another way (a state monad is overkill for this problem):

data Writer' w a = Writer' [w] a deriving (Eq, Show)

-- Exercise (10 points). Inspired by the definition of our writer
-- monad, complete the following definitions:

instance Monad (Writer' w) where
  return a = Writer' [] a 
  Writer' xs a >>= f = Writer' (xs ++ ys) b
						where
							Writer' ys b = f a

tell' :: [w] -> Writer' w ()
tell' xs = Writer' xs ()

-- Exercise (10 points). Complete the following definition to count recursion unfoldings:
wfib'' :: Integer -> Writer' Integer Integer
wfib'' n = do
			tell'([n])
			if n == 0 || n == 1
				then return n
				else do x <- wfib''(n-1)
					y <- wfib''(n-2)
					return(x + y)

{- Here is something else we can do. Using a timeout monad, we can limit how long we are willing to wait. -}

data Timeout a = Timeout (Integer -> Maybe (a , Integer))

runTimeout :: Timeout a -> Integer -> Maybe (a, Integer)
runTimeout (Timeout x) = x


-- Exercise (moderately hard, 10 points): implement this. Hint: This
-- is very close to the state monad.

instance Monad Timeout where
  return a = Timeout(\t -> Just(a,t))
  ma >>= f = Timeout(\t -> if t == 0 then Nothing else
								case runTimeout(ma) t of 
									Just(a, t') -> let (Timeout g) = f a in g t'
									Nothing -> Nothing)

tick :: Timeout () 
tick = Timeout(\t -> if t > 0 then Just((), t-1) else Nothing)

-- Here is an example of how this can be used:

tfib :: Integer -> Timeout Integer
tfib n = do
  tick
  if n == 0 || n == 1
    then return n
    else do
      x <- tfib (n - 1)
      y <- tfib (n - 2)
      return (x + y)

example1, example2 :: Maybe (Integer, Integer) -- Just (fib n, steps left) in case of success.
example1 = runTimeout (tfib 7) 50
example2 = runTimeout (tfib 8) 50

{--- Exercise (10 points, hard). Combination of state monad and writer
-- monad using a monoid. Will will have to learn about monoids from
-- our textbook. Complete the following definitions.

newtype WS w s a = WS (s -> (w, s, a))
runWS :: WS w s a -> s -> (w, s, a)
runWS (WS f) = f

instance Monoid w => Monad (WS w s) where
  return a = WS (\s -> (mempty,s,a)) 
  WS f >>= g = WS(\s -> let (w,s',a) = f s
				            (WS h) = g (write w w' s')
				               where
				                h = w' s'' a' 
						in h s'
							where 
								write :: w -> w -> s -> WS w s a
								write w w' s' = WS(w++w',s',())			
								
							
tellWS :: w -> WS w s ()
tellWS xs = WS (\s -> (xs,s,()))

putWS :: Monoid w => s -> WS w s ()
putWS s = WS (\s -> (mempty,s,()))

getWS :: Monoid w => WS w s s
getWS = WS(\s -> (mempty,s,s))-}

