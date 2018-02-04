\documentclass{article}
\title{Chapter 2 - Types and Functions}
\usepackage{parskip}
\usepackage{amsmath}
%include polycode.fmt
\begin{document}

\maketitle

\section{Haskell's type system}
Types provide \emph{abstraction} - we can know something is a string without having to understand the underlying implementation of strings.

Haskell's type system is:

\begin{itemize}
\item{\textbf{Strong} - here meaning that you can only write expressions that obey Haskell's typing rules, and that values are not automatically cast to their correct type if they are incorrect.}
\item{\textbf{Static} - The compiler knows the types in the program at compile time, so if there are any errors, the code won't run.}
\item{\textbf{using Type inference} - declaring the types of expressions is optional, theu can also be deduced by the compiler.}
\end{itemize}

\section{Other things to note about the type system}
Function application has higher precedence than using operators, so compare 2 3  == LT is the same as (compare 2 3)  == LT

Function application is left associative, so a b c d is the same as ((a b) c) d. Type signatures are right associative, so 
$Int \to [a] \to [a]$ is the same as $Int \to ([a] \to [a])$.

To specify polymorphic types, we use a type variable, which must begin with a lowercase letter. For example, we can write functions on all lists as $[a] \to a$ or $[a] \to a$, etc. (becuse list is a polymorphic type).

 Parametric polymorphism is where the type variable is substituted with the actual type on evaluation. A parameterised type in Haskell is similar to a type variable in Java generics. C++ templates also bear a resemblance to parametric polymorphism.

The empty tuple is the unit type, (). It has one value, () and is a bit like void in other languages.

The record that we use to track an unevaluated expression is referred to as a \emph{thunk}.



\section{Examples of types}

> ex1_1 :: Bool
> ex1_1 = False

> ex1_2 :: ([String], Char)
> ex1_2 = (["foo", "bar"], 'a')

> ex1_3 :: [(Bool, [[Char]])]
> ex1_3 = [(True, []), (False, [['a']])]


\section{Functions used in this chapter}

\subsection{take}
take is a function in the Prelude. take n, applied to a list xs, returns the prefix of xs of length n, or xs itself if n > length xs:

> take' :: Int -> [a] -> [a]
> take' 0 _ = []
> take' n [] = []
> take' n (x:xs) 
>  | n < 0 = []
>  | n <= length (x:xs) = x : take' (n - 1) xs
>  | otherwise = []
 

\subsection{drop}
drop is a function in the Prelude. drop n xs returns the suffix of xs after the first n elements, or [] if n > length xs:

> drop' :: Int -> [a] -> [a]
> drop' 0 xs = xs
> drop' n [] = []
> drop' n xs
>  | n < 0 = xs
>  | n <= length xs = drop' (n-1) (tail xs)
>  | otherwise = []

\subsection(fst and snd)
fst takes the first element of a pair

> fst' :: (a,b) -> a
> fst' (a,_) = a

snd takes the second element of the pair

> snd' :: (a,b) -> b
> snd' (_,b) = b

\subsection{null}
null indictes if a list is empty:

> null' :: [a] -> Bool
> null' [] = True
> null' _ = False

\subsection{last}
last will extract the last element of a list, which must be finite and non-empty:

> last' :: [a] -> a
> last' [] = error "empty list"
> last' [x] = x
> last' (x:xs) = last' xs


> lastButOne :: [a] -> a
> lastButOne (x : y : []) = x
> lastButOne (x : xs) = lastButOne xs
> lastButOne _ = error "Not enough elements"

if the list is empty, it throws an exception.

\maketitle
\end{document}