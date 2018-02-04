\documentclass{article}
\title{Chapter 1 - Getting Started}
\usepackage{parskip}
%include polycode.fmt
\begin{document}

\maketitle

Libraries used in this file:

> import Data.List.Split

Hello World!

> helloWorld :: IO ()
> helloWorld = putStrLn "Hello World"

Haskell is a \textbf{purely functional} language. Pure functions are those which always return the same result, no matter what the evaluation strategy is - so given the same input, they will always return the same output  They take immutable values as input and output new values. They also have no side effects and don't rely on external mutable state.

Pure functions are easier to test, as they behave consistenly in any context and we can easily state properties of iys behaviour that should always be true and test them on random inputs.

Haskell uses \textbf{lazy evaluation}, which means that computations are deferred until their results are actually needed. This is especially useful for processing streams.

Haskell is a \textbf{statically typed} language. This means that functions and variables have their types assigned when they are compiled. This makes code run mcuh faster than that of dynamic interpreted languages. It also uses \textbf{type inference}, so if the user omits types from the code, it can tell what types to use. This means that you don't need to spend time specifying types, but keep the performance benefit of a statically typed language.

Haskell has libraries for many applications such as database access, XML processing, web programming, GUIs and text processing and has been used in diverse areas such as verifying vehicle systems and imvestment banking.

\section{The Glasgow Haskell Compiler (GHC)}
GHC is the most widely used Haskell compiler. Real World Haskell is based on version 6.8.2. The current version is 8.0.2 and there have been some significant changes which we will discover as we get further into the book, particularly when we study monads. 

ghci is the interative interpreter of ghc and runghc can be used to run Haskell programs as scripts without first compiling them. It can be used for basic arithmetic, boolean logic and defining varibles and functions. 

\section{Functions used in the exercises}

All of the functions used in the exercise can be found in the standard Prelude. This is where all the functions that do not have to be imported into a Haskell program by default live.

\subsection{interact}

We define \emph{interact'} to show the type signature:

> interact' :: (String -> String) -> IO ()
> interact' = undefined

The interact function takes a function of type $String \to String$ as its argument. The standard input is the argument and the output is a string displayed in the standard output. 

\subsection{lines}

We define \emph{lines'} to show the type signature (note this is not the actual Haskell definition of the function):

> lines' :: String -> [String]
> lines' s = splitOn "\n" s 

The lines function breaks a string into a list of strings at the new line character.

\subsection{words}

We define \emph{words'} to show the type signature (note this is not the actual Haskell definition of the function):

> words' :: String -> [String]
> words' s = splitOneOf " \n" s 

The words function breaks a string up into a list of words, which were delimited by white space.

\subsection{length}

We define \emph{length'} to show the type signature (note this is not the actual Haskell definition of the function):

> length' :: [a] -> Int
> length' [] = 0
< length' x:xs = 1 + length' xs

The length function returns the length of a finite list as an Int. It is an instance of the more general Data.List.genericLength, the result type of which may be any kind of number.

\subsection{show}
The show function converts values to a readable string. Its type signature is:

> show' :: a -> String
> show' = undefined


\section{Exercises}

Here is the function from the book that gets the number of lines in a document:

> noOfLines :: IO ()
> noOfLines = interact lineCount
>              where lineCount input = show (length (lines' input))


We can alter it in the following ways

\begin{enumerate}

\item{This function gets the number of words in a document:

> main :: IO ()
> main = interact wordCount
>               where wordCount input = show (length (words' input))

}

\item{This function gets the number of characters in a document:

> noOfChars :: IO ()
> noOfChars = interact charCount

> charCount :: Foldable t => t a -> String 
> charCount input = show (length input)

}

\end{enumerate}

\end{document}