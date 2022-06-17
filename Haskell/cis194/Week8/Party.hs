module Party where

import Data.Tree
import Employee

instance Semigroup GuestList where
    GL l1 n <> GL  l2 m = GL (l1 ++ l2) (n+m)

instance Monoid GuestList where
    mempty = GL [] 0

instance Show GuestList where
    show (GL l f) = "Total fun: " ++ show f ++ "\n" ++ unlines (map show l)  

instance Show Employee where
    show e = empName e

-- glCons adds an Employee to the GuestList (updating the cached Fun score appropriately)
glCons :: Employee -> GuestList -> GuestList
glCons e (GL ls n) = GL (ls ++ [e]) (n + empFun e)

-- moreFun takes two GuestLists and returns whichever one of them is more fun
moreFun :: GuestList -> GuestList -> GuestList
moreFun l1@(GL _ n) l2@(GL _ m)
   | n >= m = l1
   | otherwise = l2

-- data Tree a = Node {
-- rootLabel :: a, -- label value
-- subForest :: [Tree a] -- zero or more child trees
-- }

-- treeFold is a fold function for rose trees
treeFold :: b -> (a -> [b] -> b) -> Tree a -> b
treeFold e f (Node v ls) = f v (map (treeFold e f) ls)

--killFun reduces an employee's fun to 0 :(
killFun :: Employee -> Employee
killFun (Emp n f) = Emp n 0

--nextLevel returns a pair of GuestLists: the best possible guest list with the boss and the best possible guest list without the boss
nextLevel :: Employee -> [(GuestList, GuestList)] -> (GuestList, GuestList)
nextLevel boss [] = (glCons boss mempty, glCons boss mempty)
nextLevel boss gl = (bestBoss, bestNoBoss)
    where
        killAllFun (GL ls f) = GL (map killFun ls) 0
        addBoss l = glCons boss (killAllFun l)
        bestBoss = maximum $ map (addBoss . fst) gl
        bestNoBoss = maximum $ map fst gl

-- maxFun h takes a company hierarchy as input and outputs a fun-maximizing guest list
maxFun :: Tree Employee -> GuestList
maxFun t = uncurry moreFun pair
  where
    pair = treeFold (mempty, mempty) nextLevel t

main :: IO ()
main = do
    input <- readFile "./company.txt"
    let tree = read input :: Tree Employee
    print (maxFun tree)