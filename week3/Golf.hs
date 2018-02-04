module Golf where

--Exercise 1--

--gets all factors of n in the list 
factors :: [Int] -> Int -> [Int]
factors l n = filter (\x -> x `mod` n == 0) l

-- (-1) on every integer in the list 
minusOneList :: [Int] -> [Int]
minusOneList l = map (\x -> x - 1) l

--given a list and list of positions it takes those positions and makes a new list
chooseList :: [a] -> [Int] -> [a]
chooseList l n = map (\x ->  l !! x) n

--The first list in the output should be the same as the input list. The second list in the output should contain every second element from the input list. . . and the nth list in the output should contain every nth element from the input list.
skips :: [a] -> [[a]]
skips l = map (\x -> help x) [1 .. (length l)]
 where
   help n = chooseList l (minusOneList (factors [1 .. (length l)] n))

--Exercise 2--
--returns only elements greater than the ones before and after it
localMaxima :: [Integer] -> [Integer]
localMaxima (x:y:z:ns) 
     | (y > x) && (y > z) = y: localMaxima (y:z:ns)
     | otherwise = localMaxima (y:z:ns)
localMaxima _ = []
       
--Exercise 3--
histogram :: [Integer] -> String
histogram l = allTheThings (allPos l) (maximum (allPos l)) ++ "==========\n0123456789\n"

--make a list where we count the numbers of everything
allPos :: [Integer] -> [Integer]
allPos l = map (\x -> count x l) [0..9]
 where
    count :: Integer -> [Integer] -> Integer
    count n [] = 0
    count n (a:as)
     | (n == a) = 1 + count n as
     | otherwise = count n as

--get the stars for every number between 1 and height of chart
allTheThings :: [Integer] -> Integer ->  String
allTheThings l max= concat (reverse (map (\x -> thing l x) [1..max]))
  where
    --print a star if we get to a number in the list that is >= the given number
    thing :: [Integer] -> Integer -> String
    thing [] n = "\n"
    thing (a:as) n
       | (a >= n) && (a > 0) = "*" ++ thing as n
       | otherwise =  " " ++ thing as n


 
