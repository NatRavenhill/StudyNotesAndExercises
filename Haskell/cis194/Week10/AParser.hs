module AParser where

import Control.Applicative

import Data.Char
import Data.Text.Internal.Read (IParser(runP))
import Data.Maybe

-- A parser for a value of type a is a function which takes a String
-- represnting the input to be parsed, and succeeds or fails; if it
-- succeeds, it returns the parsed value along with the remainder of
-- the input.
newtype Parser a = Parser { runParser :: String -> Maybe (a, String) }

-- For example, 'satisfy' takes a predicate on Char, and constructs a
-- parser which succeeds only if it sees a Char that satisfies the
-- predicate (which it then returns).  If it encounters a Char that
-- does not satisfy the predicate (or an empty input), it fails.
satisfy :: (Char -> Bool) -> Parser Char
satisfy p = Parser f
  where
    f [] = Nothing    -- fail on the empty input
    f (x:xs)          -- check if x satisfies the predicate
                        -- if so, return x along with the remainder
                        -- of the input (that is, xs)
        | p x       = Just (x, xs)
        | otherwise = Nothing  -- otherwise, fail

-- Using satisfy, we can define the parser 'char c' which expects to
-- see exactly the character c, and fails otherwise.
char :: Char -> Parser Char
char c = satisfy (== c)

{- For example:

*Parser> runParser (satisfy isUpper) "ABC"
Just ('A',"BC")
*Parser> runParser (satisfy isUpper) "abc"
Nothing
*Parser> runParser (char 'x') "xyz"
Just ('x',"yz")

-}

-- For convenience, we've also provided a parser for positive
-- integers.
posInt :: Parser Integer
posInt = Parser f
  where
    f xs
      | null ns   = Nothing
      | otherwise = Just (read ns, rest)
      where (ns, rest) = span isDigit xs

------------------------------------------------------------
-- Your code goes below here
------------------------------------------------------------
first :: (a -> b) -> (a,c) -> (b,c)
first f (x,y) = (f x, y)

instance Functor Parser where
    fmap f p = Parser $ fmap (first f) . runParser p

instance Applicative Parser where
    pure a = Parser (\s -> Just (a, s))
    p1 <*> p2 = Parser f
        where
            f x = case runParser p1 x of
                        Nothing -> Nothing
                        Just (g, s) -> runParser (g <$> p2) s

-- abParser expects to see the characters ’a’ and ’b’ and returns them as a pair
abParser :: Parser (Char, Char)
abParser = ((,) <$> aParser)  <*> bParser
    where
        aParser = char 'a'
        bParser = char 'b'

-- abParser_ acts in the same way as abParser but returns ()
abParser_ :: Parser ()
abParser_ = const () <$> abParser

--intPair which reads two integer values separated by a space and returns the integer values in a list
intPair :: Parser [Integer]
intPair = Parser f
    where
        f s
            | isNothing part1 || isNothing part2 || isNothing part3 = Nothing
            | otherwise = Just ([res1, res2], "")
            where
                part1 = runParser posInt s
                res1 = fst $ fromJust part1
                part2 = runParser (char ' ') (snd $ fromJust part1)
                part3 = runParser posInt (snd $ fromJust part2)
                res2 = fst $ fromJust part3


intPair' :: Parser [Integer]
intPair' = (((\a _ b -> [a, b]) <$> posInt) <*> char ' ') <*> posInt

instance Alternative Parser where
    empty = Parser (const Nothing)
    p1 <|> p2 =  Parser f
        where
            f s = runParser p1 s <|> runParser p2 s

--intOrUppercase parses either an integer value or an uppercase character, and fails otherwise.
intOrUppercase :: Parser ()
intOrUppercase = (const () <$> posInt) <|> (const () <$> satisfy isUpper)
