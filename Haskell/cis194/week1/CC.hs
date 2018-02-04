module CC where

--Exercise 1

--test credit card numbers
test = 4012888888881881
test2 = 4012888888881882

--gets last digit of a given integer
lastDigit :: Integer -> Integer
lastDigit n = n - ((n `div` 10) * 10)

--subtracts last digit from a given integer
prepareNext :: Integer -> Integer
prepareNext n = (n - lastDigit n) `div` 10

--converts a number to a list of digits
toDigits :: Integer -> [Integer]
toDigits n
  | (n <= 0) = []
  | otherwise = toDigits (prepareNext n) ++ [lastDigit n]

--converts a number to a list of digits in reverse
toDigitsRev :: Integer -> [Integer]
toDigitsRev n 
  | (n <= 0) = []
  | otherwise  = [lastDigit n] ++ toDigitsRev (prepareNext n)


--Exercise 2

--double every other entry, starting from the right
--if length is even, double this one else don't
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther [] =[]
doubleEveryOther (hd : tl)
  | ((length (hd : tl)) `mod` 2) == 0 = (hd * 2) : doubleEveryOther tl  
  | otherwise = hd : doubleEveryOther tl

--Exercise 3

--Calucluates the sum of all digits in the list
sumDigits :: [Integer] -> Integer
sumDigits l = sumD (concatMap toDigits l)
 where
   sumD [] = 0
   sumD (hd : tl) = hd + sumD tl

--Exercise 4
--doubles value of every second digit beginning from the right.
--add the digits of the doubled and undoubled values
--if remainder when divided by 10 is 0, then number is valid
validate :: Integer -> Bool
validate n = (sumDigits (doubleEveryOther (toDigits n)) `mod` 10) == 0
