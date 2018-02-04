data List a = Cons a (List a) | Nil
            deriving (Show)

toList :: List a -> [a]
toList Nil = []
toList (Cons x xs) = x : toList xs

data Tree a = Maybe (a (Maybe Tree a) (Maybe Tree a))

