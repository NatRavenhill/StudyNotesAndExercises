module SExpr where

import AParser
import Control.Applicative
import Data.Text.Internal.Read (IParser(runP))
import Data.Char
import Data.Maybe

------------------------------------------------------------
--  1. Parsing repetitions
------------------------------------------------------------

--zeroOrMore takes a parser as input and runs it consecutively as many times as possible (which could be none, if it fails right away), returning a list of the results.
zeroOrMore :: Parser a -> Parser [a]
zeroOrMore = many

--oneOrMore is similar, except that it requires the input parser to succeed at least once
oneOrMore :: Parser a -> Parser [a]
oneOrMore  = some

------------------------------------------------------------
--  2. Utilities
------------------------------------------------------------

--spaces parses a consecutive list of zero or more whitespace characters
spaces :: Parser String
spaces = zeroOrMore (satisfy isSpace)

--ident parses an identifier (an alphabetic character followed by zero or more alphanumeric characters)
ident :: Parser String
ident =  liftA2 (:) (satisfy isAlpha) $ oneOrMore (satisfy isAlphaNum)

------------------------------------------------------------
--  3. Parsing S-expressions
------------------------------------------------------------

-- An "identifier" is represented as just a String; however, only
-- those Strings consisting of a letter followed by any number of
-- letters and digits are valid identifiers.
type Ident = String

-- An "atom" is either an integer value or an identifier.
data Atom = N Integer | I Ident
  deriving Show

-- An S-expression is either an atom, or a list of S-expressions.
data SExpr = A Atom
           | Comb [SExpr]
  deriving Show

parseN :: Parser Atom
parseN = N <$> posInt

parseI :: Parser Atom
parseI = I <$> ident

parseA :: Parser SExpr
parseA = A <$> (parseN <|> parseI)

parseComb :: Parser SExpr
parseComb = char '(' *> (Comb <$> oneOrMore parseExpr) <* char ')'

parseExpr :: Parser SExpr
parseExpr = spaces *> parseA <|> parseComb <* spaces

-----