module Week4 where

import Data.Char

data Tree' = Leaf Int 
           |  Branch Tree' Tree'
           deriving (Eq, Show)

data Token = ConstantT Int 
           | LeafT
           | BranchT
           | OpenBracketT
           | CloseBracketT
           deriving (Eq, Show)
                    
{-                     

 Exercise (30 points). Define the following function:

-}
wordDelimiter :: Char -> Bool                                 
wordDelimiter c = c == '(' || c == ')' || isSpace c

wordEnd :: String -> Bool
wordEnd [] = True
wordEnd (x:xs) = wordDelimiter x
                                 
lexicalError :: String -> a                   
lexicalError xs = error ("Lexical error: " ++ xs)

lexicalAnalysis :: String -> [Token]
lexicalAnalysis [] = []
lexicalAnalysis ('(':xs) = OpenBracketT : lexicalAnalysis xs
lexicalAnalysis (')':xs) = CloseBracketT : lexicalAnalysis xs
lexicalAnalysis ('L':'e':'a':'f':xs)
	|wordEnd xs = LeafT : lexicalAnalysis xs
	|otherwise	= lexicalError("Leaf" ++ xs)
lexicalAnalysis ('B':'r':'a':'n':'c':'h':xs)
	|wordEnd xs = BranchT : lexicalAnalysis xs
	|otherwise	= lexicalError("Branch" ++ xs)
lexicalAnalysis (x:xs) 
           | isDigit x || x == '-' = ConstantT n : lexicalAnalysis zs
           where 
             (ys, zs) = span isDigit xs
             n = read(x : ys)    

lexicalAnalysis (x:xs) 
           | isSpace x = lexicalAnalysis xs

lexicalAnalysis (x:xs) 
           | otherwise = lexicalError(x:xs)
{-                     

 Exercise (30 points). Define the following function:

-}

syntacticalAnalysis :: [Token] -> Tree'
syntacticalAnalysis xs = 
  let (t, ys) = scanTree xs 
  in case ys of
       [] -> t
       _  -> syntaxError("Expected end of input but found " ++ show ys)

scanTree ::[Token] -> (Tree', [Token])
scanTree [] = syntaxError("Expected a tree but found end of input")

scanTree(BranchT:xs) = (Branch l r, zs)
	where
	 (l, ys) = scanTree xs
	 (r, zs) = scanTree ys
	 
scanTree(LeafT: xs) = (Leaf n, zs)
	where
	(n, zs) = scanInt xs
	
scanTree (OpenBracketT:xs) = (t, zs)  
  where
    (t, ys) = scanTree xs
    zs = scanCloseBracket ys

scanTree xs = syntaxError("Expected a tree but found " ++ show xs)    
	
scanInt :: [Token] -> (Int, [Token])                 
scanInt [] = syntaxError "Expected integer but got end of input"
scanInt (ConstantT n : xs) = (n, xs)
scanInt xs = syntaxError("Expected integer but found " ++ show xs)

scanCloseBracket :: [Token] -> [Token]
scanCloseBracket [] = syntaxError "Expected ')' but found end of input"
scanCloseBracket (CloseBracketT : xs) = xs
scanCloseBracket (x:xs) = syntaxError("Expected ')' but found " ++ show(x:xs))
	
syntaxError :: String -> a                   
syntaxError xs = error ("Syntax error: " ++ xs)

{-

 Combining the two functions we get a parser:

-}

parseTree' :: String -> Tree'
parseTree' = syntacticalAnalysis . lexicalAnalysis    
