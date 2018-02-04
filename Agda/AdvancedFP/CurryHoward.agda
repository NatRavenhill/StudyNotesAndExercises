module CurryHoward where

-- PART 1 : BOOLEAN VALUES

data Bool : Set where
  true  : Bool
  false : Bool

not : Bool → Bool
not true = false
not false = true

_&_ : Bool → Bool → Bool
true & true = true
_ & _ = false

infixr 6 _&_

_||_ : Bool → Bool → Bool
false || false = false
_ || _ = true

infixr 5 _||_

if_then_else_ : Bool → Bool → Bool → Bool
if true then x else _ = x
if false then _ else x = x 

-- Exercise 1 : Define EXCLUSIVE OR _⊕_
_⊕_ : Bool → Bool → Bool
true ⊕ false = true
false ⊕ true = true
_ ⊕ _ = false

-- PART 2 : PROPOSITIONAL LOGIC

data ⊤ : Set where
  tt : ⊤ 

data ⊥ : Set where

data _∧_ (A B : Set) : Set where
  _,_ : A → B → A ∧ B

infixr 6 _∧_

data _∨_ (A B : Set) : Set where
  inl : A → A ∨ B
  inr : B → A ∨ B

infixr 5 _∨_

-- Hilbert-style laws

then-1 : {A B : Set} → A → B → A
then-1 a b = a

then-2 : {A B C : Set} → (A → B → C) → (A → B) → (A → C)
then-2 f g a = f a (g a)

and-1 : {A B : Set} → A ∧ B → A
and-1 (a , b) = a

and-2 : {A B : Set} → A ∧ B → B
and-2 (a , b) = b

and-3 : {A B : Set} → A → B → A ∧ B
and-3 a b = a , b

-- Exercise 2 : Complete Hilbert-style laws

or-1 : {A B : Set} → A → A ∨ B
or-1 a = inl a

or-2 : {A B : Set} → B → A ∨ B
or-2 b = inr b

or-3 : {A B C : Set} → (A → C) → (B → C) → (A ∨ B → C)
or-3 f g (inl a) = f a
or-3 f g (inr b) = g b

falsity : {A : Set} → ⊥ → A
falsity = λ {A} → λ ()

-- PART 3 : NEGATION

¬ : Set → Set 
¬ A = A → ⊥

not-1 : {A B : Set} → (A → B) → (A → ¬ B) → ¬ A
not-1 p q a = q a (p a)

-- Exercise 3
not-2 : {A B : Set} → A → ¬ A → B
not-2 a p = falsity (p a)

-- Exercise 4 : De Morgan Laws (which one is not true?)
de-morgan-1 : {A B : Set} → ¬ A ∧ ¬ B → ¬ (A ∧ B)
de-morgan-1 (na , nb) (a , b) = na a

de-morgan-2 : {A B : Set} → ¬ (A ∨ B) → ¬ A ∧ ¬ B
de-morgan-2 p = (λ a → p (inl a)) , (λ b → p (inr b))

de-morgan-3 : {A B : Set} → ¬ A ∨ ¬ B → ¬ (A ∧ B)
de-morgan-3 (inl x) (a , b) = x a
de-morgan-3 (inr x) (a , b) = x b

--This can't be proved intuitionistically. We are given a proof that (A ∧ B) is empty but this does not mean that both A and B are empty, it may be that just A is empty but B is not

de-morgan-4 : {A B : Set} → ¬ (A ∧ B) → ¬ A ∧ ¬ B
de-morgan-4 nab = (λ a → {!!}) , (λ b → {!!})


-- PART 4 : CLASSICAL LOGIC

postulate LEM : (A : Set) → A ∨ ¬ A

classical-1 : {A : Set} → ¬ (¬ A) → A
classical-1 = {!!}
 

classical-1' : {A : Set} → ¬ (¬ A) → A
classical-1' = {!!} -- no 'with' pattern

-- Exercise 5 : Use double negation to prove exclude middle

postulate DEE : {A : Set} → ¬ (¬ A) → A

classical-2 : {A : Set} → A ∨ ¬ A
classical-2 = {!!}


