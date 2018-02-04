module Hanoi where

type Peg = String
type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 0 _ _ _ = []
hanoi n a b c = hanoi (n-1) a c b ++ --move n-1 from a to b with c as temporary storage
                          [(a,b)]  ++ --move top disk from a to b
                          hanoi (n-1) c b a --move n-1 disks from c to b using a as temporary storage

