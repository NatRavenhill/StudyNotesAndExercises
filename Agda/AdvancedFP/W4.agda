module W4 where

open import Data.List
open import Data.Bool
open import Data.Nat
open import Data.Product
open import Data.Empty
open import Relation.Binary.PropositionalEquality

--equality of lists

--as a function 
_~_ : {A : Set} → List A → List A → Bool
[] ~ [] = true
[] ~ (_ ∷ _) = false
(_ ∷ _) ~ [] = false 
(_ ∷ xs) ~ (_ ∷ ys) = xs ~ ys
infix 5 _~_

ex1 : Bool
ex1 = 1 ∷ 2 ∷ [] ~ 3 ∷ 4 ∷ []

--as a type
data _≈_ {A : Set} : List A → List A → Set where
  equi-nil : [] ≈ []
  equi-cons : ∀ x y xs ys → xs ≈ ys → (x ∷ xs) ≈ (y ∷ ys)
infix 5 _≈_

ex2 : 1 ∷ 2 ∷ [] ≈ 3 ∷ 4 ∷ []
ex2 = equi-cons (suc zero) (suc (suc (suc zero))) (suc (suc zero) ∷ [])
        (suc (suc (suc (suc zero))) ∷ [])
        (equi-cons (suc (suc zero)) (suc (suc (suc (suc zero)))) [] []
         equi-nil)

--proof that function = type
~→≈ : {A : Set} → (xs ys : List A) → xs ≈ ys → xs ~ ys  ≡ true
~→≈ .[] .[] equi-nil = refl
~→≈ .(x ∷ xs) .(y ∷ ys) (equi-cons x y xs ys p) = ~→≈ xs ys p

--proof that type = function
≈→~ : {A : Set} → (xs ys : List A) → xs ~ ys ≡ true → xs ≈ ys
≈→~ [] [] r = equi-nil
≈→~ [] (x ∷ ys) ()
≈→~ (x ∷ xs) [] ()
≈→~ (x ∷ xs) (y ∷ ys) r = equi-cons x y xs ys (≈→~ xs ys r)

--if xs ≈ xs' and ys ≈ ys' then xs ++ ys ≈ xs' ++ ys', appending two lists will be equal to appending their equal lists
cong≈ : {A : Set} → (xs xs' ys ys' : List A) → xs ≈ xs' → ys ≈ ys' → xs ++ ys ≈ xs' ++ ys'
cong≈ [] [] ys ys' p q = q
cong≈ [] (x ∷ xs') .[] .[] () equi-nil
cong≈ [] (x ∷ xs') .(x₁ ∷ xs) .(y ∷ ys) () (equi-cons x₁ y xs ys q)
cong≈ (x ∷ xs) [] ys ys' () q
cong≈ (x ∷ xs) (x₁ ∷ xs') ys ys' (equi-cons .x .x₁ .xs .xs' p) q = equi-cons x x₁ (xs ++ ys) (xs' ++ ys') (cong≈ xs xs' ys ys' p q)


{- EXERCISE 1-} 
cong~ : {A : Set} → (xs xs' ys ys' : List A) → xs ~ xs' ≡ true → ys ~ ys' ≡ true → xs ++ ys ~ xs' ++ ys' ≡ true
cong~ [] [] ys ys' p q = q 
cong~ [] (x ∷ xs') ys ys' () q
cong~ (x ∷ xs) [] ys ys' () q
cong~ (x ∷ xs) (x' ∷ xs') ys ys' p q = cong~ xs xs' ys ys' p q


even : ℕ → Bool
even zero = true
even (suc zero) = false
even (suc (suc n)) = even n

--can't pattern match on p so use with notation
{-
even-s n p with even n
even-s n p | true = {!!} -- don't want to pattern match on p, just get n : ℕ which cannot be used to complete the proof, therefore use inspect pattern
even-s n () | false  -- no solution so use empty
-}

--inspect allows to introduce an extra equality,  then can use to unify variables:
even-s : ∀ n → even n ≡ true → even (suc n) ≡ false
even-s n p with even n | inspect even n 
even-s zero refl | true | [ eq ] = refl
even-s (suc zero) refl | true | [ () ] --eq was absurd so is eliminated by agda
even-s (suc (suc n)) refl | true | [ eq ] = even-s n eq --ignore reveal and hide and just reas as even n ≡ true
even-s n () | false | eq 

-- ∃ --dependent pair

--for any Bool there exists another Bool 
ex1' : (b : Bool) → ∃ λ b'  →  b ≡ b' → ⊥
ex1' true = false , (λ ())
ex1' false = true , (λ ())

surj∧ : (b : Bool) → ∃ λ b' → ∃ λ b'' → b' ∧ b'' ≡ b
surj∧ true = true , true , refl   -- true ∧ true ≡ true reduces to refl
surj∧ false = true , false , refl  -- true ∧ false ≡ false reduces to refl

open import Data.Sum --disjoint union
 
{- EXERCISE 2 -}

--For any x : A ⊎ B there is either a y : A or a y : B equal to it (stability property)
stab⊎ : {A B : Set} → (x : A ⊎ B) → (∃ λ (y : A) → x ≡ (inj₁ y)) ⊎ (∃ λ (y : B) → x ≡ (inj₂ y))
stab⊎ (inj₁ x) = inj₁ (x , refl)
stab⊎ (inj₂ y) = inj₂ (y , refl)

