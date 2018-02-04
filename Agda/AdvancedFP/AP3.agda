module AP3 where

data Nat : Set where
  zero : Nat
  suc : Nat → Nat

₀ = zero
₁ = suc ₀
₂ = suc ₁
₃ = suc ₂

--addition function
_+_ : Nat → Nat → Nat
zero + n = n
suc m + n = suc (m + n)
infixl 5 _+_

--polymorphic list 
data ListNat : Set where
   nil : ListNat
   _::_ : Nat → ListNat → ListNat

infixr 4 _::_

{-Parameterised type -} --list is uniformly parameterised over A (A can be used in the definition)
data List (A : Set) : Set where
   [] : List A
   _∷_ : A → List A → List A

infixr 4 _∷_

list1 : ListNat
list1 = ₀ :: ₁ :: ₂ :: nil

list1' : List Nat
list1' = ₀ ∷ ₁ ∷ ₂ ∷ []

length : {A : Set} → List A → Nat
length [] = zero
length (x ∷ xs) = suc (length xs)

{-Indexed type-}
data Vector (A : Set) : Nat → Set where
  nilv : Vector A zero
  _:::_ : {n : Nat} → A → Vector A n → Vector A (suc n) 
infixr 4 _:::_

list1'' : Vector Nat ₂
list1'' = ₀ ::: ₁ ::: nilv

{- Indexed types as PREDICATES -}

data _≡_  {A : Set} : A → A → Set where
   refl : (x : A) → x ≡ x
infix 0 _≡_ --binds most weakly

example0 : ₂ ≡ ₁ + ₁
example0 = refl ₂ -- or refl (suc (suc zero))


--Properties of ≡ --

trans-≡ : {A : Set} →  (x y z : A)  → x ≡ y → y ≡ z → x ≡ z -- can use ∀ if type is automatically inferred
trans-≡ x .x .x (refl .x) (refl .x) = refl x

sym-≡ : {A : Set} → (x y : A) → x ≡ y → y ≡ x
sym-≡ x .x (refl .x) = refl x

--congruence property of functions
≡-cong-f : {A B : Set} → (x y : A) (f : A → B) → x ≡ y → f x ≡ f y
≡-cong-f x .x f (refl .x) = refl (f x)

--proofs by induction on Nats
--defined separately because suc is a construtor, not a function!
suc-cong : ∀ m n → m ≡ n → suc m ≡ suc n
suc-cong m .m (refl .m) = refl (suc m)

--Proofs by induction on Nat

right-zero : ∀ m → m + zero ≡ m
right-zero zero = refl zero
right-zero (suc m) = suc-cong _ _  IH
  where
    IH : m + zero ≡ m
    IH = right-zero m

ex3 : {A : Set} → (xs : List A) (x : A) (n : Nat) → length xs ≡ n → length (x ∷ xs) ≡ suc n
ex3 xs x .(length xs) (refl .(length xs)) = refl (suc (length xs))

--BIGGER EXAMPLE 1 ( on natural numbers)
comm-+ : ∀ m n → m + n ≡ n + m
comm-+ zero n = sym-≡ _  _ (right-zero n)
comm-+ (suc m) n = Goal
 where
 IH : m + n ≡ n + m
 IH = comm-+ m n

 lemma1 : ∀ m n → m + suc n ≡ suc (m + n)
 lemma1 zero n = refl (suc n)
 lemma1 (suc m) n = suc-cong _  _(lemma1 m n)
 
 Goal : suc (m + n) ≡ n + suc m
 Goal = trans-≡ _ _ _  (suc-cong _ _  IH) (sym-≡ _ _ (lemma1 n m))

--BIGGER EXAMPLE 2 (for lists) 
--definition of concatenation (++) for lists:
_++_ : {A : Set} → List A → List A → List A
[] ++ ys = ys
(x ∷ xs) ++ ys = x ∷ (xs ++ ys)
infixl 3 _++_

--definition of a list containg one element
[_] : {A : Set} → A → List A
[ x ] = x ∷ []

--reversal of a list
rev : {A : Set} → List A → List A
rev [] = []
rev (x ∷ xs) = rev xs ++ [ x ] 

--congruence property for prepending an element to a list
::-cong : {A : Set} → (x : A) → (xs ys : List A) → xs ≡ ys → x ∷ xs ≡ x ∷ ys
::-cong x xs .xs (refl .xs) = refl (x ∷ xs)

--congruence property for concatenating lists
++-cong : {A : Set} → (x y xs : List A) → x ≡ y → x ++ xs ≡ y ++ xs
++-cong x .x xs (refl .x) = refl (x ++ xs)

---second congruence property for concatenating lists
++-cong' : {A : Set} → (x y xs : List A) → x ≡ y → xs ++ x ≡ xs ++ y
++-cong' x .x xs (refl .x) = refl (xs ++ x)

--proof of commutavity of concatenating any list and an empty list
++-[]-comm : {A : Set} → (x : List A) → x ++ [] ≡ [] ++ x
--base case reduces to [] = [], so is just reflexivity
++-[]-comm [] = refl []
--inductive case
++-[]-comm (x ∷ xs) = Goal
   where
     --inductive hypothesis
     IH : xs ++ [] ≡ [] ++ xs
     IH = ++-[]-comm xs
   
     Goal : x ∷ (xs ++ []) ≡ x ∷ xs
     Goal = trans-≡ _ _ _ sub1 sub2
       where
          --replace (xs ++ []) with ([] ++ xs) using ∷ congruence and inductive hypothesis
          sub1 :  x ∷ (xs ++ []) ≡ x ∷ ([] ++ xs)
          sub1 = ::-cong x (xs ++ []) ([] ++ xs) IH
          --replace  ([] ++ xs) with xs using :: congruence and reflexivity
          sub2 : x ∷ ([] ++ xs) ≡ x ∷ xs
          sub2 = ::-cong x ([] ++ xs) xs (refl xs)

--proof of associativity of ++
++-assoc : {A : Set} → (x y z : List A) → x ++ y ++ z ≡ x ++ (y ++ z)
--base case, evaluates to refl
++-assoc [] y z = refl (y ++ z)
++-assoc (x ∷ xs) y z = goal
    where
     --inductive hypothesis
     IH : xs ++ y ++ z ≡ xs ++ (y ++ z)
     IH = ++-assoc xs y z

     goal :  x ∷ xs ++ y ++ z ≡ x ∷ xs ++ (y ++ z)
     goal = trans-≡ _ _ _ p1' p2'
       where
         --equal by definition of ++ so just reflexivity
         p1' : x ∷ xs ++ y ++ z ≡  x ∷ (xs ++ y) ++ z
         p1' = refl (x ∷ (xs ++ y ++ z))
        --replace (xs ++ y) ++ z with xs ++ (y ++ z) using congruence of :: and inductive hypothesis
         p2' : x ∷ (xs ++ y) ++ z ≡ x ∷ xs ++(y ++ z)
         p2' = ::-cong x ((xs ++ y) ++ z) (xs ++ (y ++ z)) IH

--proof of distributivity of rev and ++. Given rev (xs ++ ys) we can equivalently have (rev ys) ++ (rev xs). They must be in this order because of the definition of rev (it is not commutative):
rev-++-distr : {A : Set} → (xs ys : List A) → rev (xs ++ ys) ≡ (rev ys) ++ (rev xs)
--base case
rev-++-distr [] ys = trans-≡ _ _ _ (trans-≡ _ _ _ p1 p2) p3
          where
           --this is equal by the defintion of ++, so is just reflexivity
            p1 : rev ([] ++ ys) ≡ rev ys
            p1 = refl (rev ys)
 
            p2 : rev ys ≡ rev ys ++ []
            p2 = id ys 
              where
              --we prove this by induction:
              id : {A : Set} → (xs : List A) → rev xs ≡ rev xs ++ rev []
              --base case is reduces to  [] = [], so is just reflexivity 
              id [] = refl []
              --inductive case
              id (x ∷ xs) = Goal'
                 where
                  --inductive hypothesis
                  IH' :  rev xs ≡ rev xs ++ rev []
                  IH' = id xs
                  --because ++ is defined on [] ++ x and not x ++ [] it is easier to prove the inverse statement, which we do using symmetry:
                  Goal' : rev (x ∷ xs) ≡ rev (x ∷ xs) ++ rev []
                  Goal' =  sym-≡ _ _ (trans-≡ _ _ _ sub1 sub2)
                       where
                        --this is just the commutativty of ++ on (rev xs ++ [ x ]) and [] because we are switching the order of the parameters
                       sub1 : (rev xs ++ [ x ]) ++ [] ≡ [] ++ (rev xs ++ [ x ])
                       sub1 = ++-[]-comm (rev xs ++ [ x ])
                       --then this is the same by evalutaing the first ++, so is just reflexivity
                       sub2 :  [] ++ (rev xs ++ [ x ]) ≡ rev xs ++ [ x ]
                       sub2 = refl (rev xs ++ x ∷ [])
           --now we evaluate the second ++, which is the same as the goal, so this is just reflexivity
            p3 : rev ys ++ [] ≡ rev ys ++ rev []
            p3 = refl (rev ys ++ [])
            
--inductive case for distribuivity
rev-++-distr (x ∷ xs) ys = Goal'
         where
            --inductive hypothesis
            IH'' : rev (xs ++ ys) ≡ (rev ys) ++ (rev xs)
            IH'' = rev-++-distr xs ys

            Goal' : rev ((x ∷ xs) ++ ys) ≡ (rev ys) ++ (rev (x ∷ xs))
            Goal' = trans-≡ _ _ _ sub sub'
               where 
                 --replace rev (xs ++ ys) with rev xs using congruence with inductive hypothesis
                 sub : rev (xs ++ ys) ++ [ x ] ≡ rev ys ++ rev xs ++ [ x ] 
                 sub = ++-cong (rev (xs ++ ys)) (rev ys ++ rev xs) [ x ] IH''
                --get goal by using associativity of ++
                 sub' : rev ys ++ rev xs ++ [ x ]  ≡ rev ys ++ (rev xs ++ [ x ])
                 sub' = ++-assoc (rev ys) (rev xs) [ x ]

--proof of involutivity of lists
invol-rev : {A : Set} → (xs : List A) → rev (rev xs) ≡ xs
--the base case is just rev(rev []) = rev [] = [], so this is just refl []
invol-rev [] = refl []
invol-rev (x ∷ xs) = Goal
 where
--inductive hypothesis is the return type with just xs
 IH : rev (rev xs) ≡ xs
 IH = invol-rev xs

--then for the goal we prepend x to xs according to the constructor for lists
 Goal : rev (rev (x ∷ xs)) ≡ x ∷ xs
--we chain together the sub proofs using transivity, so it is like doing a proof line by line
 Goal = trans-≡ _ _ _ (trans-≡ _ _ _  p1 p2) (trans-≡ _ _ _ p3 p4)
   where
       --first we prove this statement because the hand side is equal to the right hand side because it is just the same with rev (x ∷ xs) evaluated, so this is refexivity
       p1 : rev (rev (x ∷ xs)) ≡ rev (rev xs ++ [ x ] )    
       p1 = refl (rev (rev xs ++ x ∷ []))
    
       --we use the distributivity of rev and ++ to show that rev (rev xs ++ [ x ]) is equal to rev [ x ]   
       p2 : rev (rev xs ++ [ x ] )  ≡ rev [ x ] ++ rev (rev xs)
       p2 = rev-++-distr (rev xs) [ x ]

       --then we use congruence to show that rev (rev xs) can be replaced with xs , by using the inductive hypothesis
       p3 : rev [ x ] ++ rev (rev xs) ≡ rev [ x ] ++ xs
       p3 = ++-cong' (rev (rev xs)) xs [ x ] IH

       --finally, we get (x ∷ xs) by evaluating rev [ x ] ++ xs, so they are equal by reflexivity 
       p4 : rev [ x ] ++ xs ≡ (x ∷ xs)
       p4 = refl (x ∷ xs)
