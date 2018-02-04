module AgdaWeek2 where


-- PROPOSITIONAL LOGIC

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


-- Hilbert-style laws (left associative)

then-1 : {A B : Set} → A → B → A
then-1 x _ = x

then-2 : {A B C : Set} → (A → B → C) → (A → B) → (A → C)
then-2 f g a = f a (g a) 

and-1 : {A B : Set} → A ∧ B → A
and-1 (x , _) = x

and-2 : {A B : Set} → A ∧ B → B
and-2 (_ , y) = y

and-3 : {A B : Set} → A → B → A ∧ B
and-3 a b = a , b

or-1 : {A B : Set} → A → A ∨ B
or-1 a = inl a

or-2 : {A B : Set} → B → A ∨ B
or-2 b = inr b

or-3 : {A B C : Set} → (A → C) → (B → C) → (A ∨ B → C)
or-3 f g (inl x) = f x
or-3 f g (inr x) = g x

falsity : {A : Set} → ⊥ → A
falsity ()

-- Negation

¬ : Set → Set 
¬ A = A → ⊥

not-1 : {A B : Set} → (A → B) → (A → ¬ B) → ¬ A
not-1 f g a = g a (f a)

not-2 : {A B : Set} → A → ¬ A → B
not-2 a f = falsity (f a)

-- De Morgan Laws (which one is not true?)
de-morgan-1 : {A B : Set} → ¬ A ∧ ¬ B → ¬ (A ∧ B)
de-morgan-1 (_ , g) (_ , b) = g b

de-morgan-2 : {A B : Set} → ¬ (A ∨ B) → ¬ A ∧ ¬ B
de-morgan-2 p = (λ z → p (inl z)) , (λ z → p (inr z))

de-morgan-3 : {A B : Set} → ¬ A ∨ ¬ B → ¬ (A ∧ B)
de-morgan-3 (inl f) (a , _) = f a
de-morgan-3 (inr f) (_ , b) = f b

-- Triple negation elimination

triple-neg : {A : Set} → ¬ (¬ (¬ A)) → ¬ A
triple-neg = λ {A} z z₁ → z (λ z₂ → z₂ z₁)

------------ W E E K   2 ------------
---------- CLASSICAL LOGIC ----------

postulate LEM : {A : Set} → A ∨ ¬ A

classical-1 : {A : Set} → ¬ (¬ A) → A
classical-1 {A} f = or-3 {A} {¬ A} {A} (λ z → z) (λ z → falsity (f z)) (LEM {A}) 

classical-1' : {A : Set} → ¬ (¬ A) → A
classical-1' {A} f with LEM {A}
classical-1' f | inl z = z
classical-1' f | inr z = falsity (f z) 

postulate DNE : {A : Set} → ¬ (¬ A) → A

-- Exercise : Use double negation to prove exclude middle
classical-2 : {A : Set} → A ∨ ¬ A
classical-2 {A} = DNE (λ x → x (inr (λ x₁ → x (inl x₁)))) 

-- Exercise : Prove the missing De Morgan's law using Classical Logic
de-morgan-4 : {A B : Set} → ¬ (A ∧ B) → ¬ A ∨ ¬ B
de-morgan-4 {A} {B} p = DNE (λ z → z (inl (λ a → z (inr (λ b → p (a , b))))))

-- Exercise : Use Constructive Logic to prove contrapositive
contrapos : {A B : Set} → (A → B) → ¬ B → ¬ A
contrapos  {A} {B} f nb a = nb (f a)

--Exercise : Use Classical Logic to prove contrapositive' 
contrapos' : {A B : Set} → (¬ A → ¬ B) → B → A
contrapos' {A} {B} p b = DNE (λ na → p na b)

-- Exercise : Use Classical Logic to prove impl
impl : {A B : Set} → (A → B) → ¬ A ∨ B
impl {A} {B} p = DNE (λ x → x (inl (λ a → x (inr (p a)))))

impl' : {A B : Set} → ¬ A ∨ B → A → B 
impl' (inl na) a = DNE (λ _ → na a)
impl' (inr b) a = b

distr-dist : {A B C : Set} → A ∨ (B ∧ C) → (A ∨ B) ∧ (A ∨ C)
distr-dist (inl a) = inl a , inl a
distr-dist (inr (b , c)) = inr b , inr c

-- Exercise : Use Classical Logic to prove Peirce's Law 
-- http://en.wikipedia.org/wiki/Peirce%27s_law#Other_proofs_of_Peirce.27s_law

-- I did this slightly differently to Dan's initial steps as I didn't look at the updated file first but as far as I am aware it should be equivalent. Each implication in the proof is represented separately, then they are chained together at the end to give the whole proof: 

--the first equality can just be proved using impl that we previously defined
p0 : {p q : Set} → ((p → q) → p) → ¬ (p → q) ∨ p
p0 f = impl f

--for p1, our parameter is a disjunction, so we give a proof for both sides of it. The proofs for both sides construct the disjunction by constructing an element of p, using the right injection. The left hand side of our parameter is a negated function so we can use DNE and assuming a proof of not p, apply our parameter (as it is a  function), assuming an elment of p. Then we use DNE again so that we can get an element of false giving not p and p. The right hand side is just p so we can use the right injection on p.
p1 : {p q : Set} → ¬ (p → q) ∨ p → ¬ (¬ p ∨ q) ∨ p
p1 (inl x) = inr (DNE (λ np → x (λ p → DNE (λ _ → np p) ))) 
p1 (inr p) = inr p

--for p2, we have a disjunction again, so we have two proofs and again we construct the disjunction using right injection on p. The left hand side is a negated disjunction so we use DNE and assume an element of ¬ p to get p. Then the right hand side is given p so we can just use this p.
p2 : {p q : Set} → ¬ (¬ p ∨ q) ∨ p → (p ∧ ¬ q) ∨ p
p2 (inl x) = inr (DNE (λ np → x (inl np))) 
p2 (inr p) = inr p

--for p3, the left hand side of the parameter is the same as the left hand side of the proof so we give it again using the left injection. The right hand side of the parameter is p, so we use this to construct (p ∧ ⊤) by using and-3 with arguments p and tt (the constructor for ⊤). Then we construct the disjunction using the right injection.

p3 : {p q : Set} → (p ∧ ¬ q) ∨ p → (p ∧ ¬ q) ∨ (p ∧ ⊤)
p3 {p} {q} (inl x) = inl x
p3 {p} {q} (inr x) = inr (x , tt)

--In the proof given in Wikipedia, this is reprsented as (p ∧ ¬ q) ∨ (p ∧ 1). I thought that 1 represented an identity and had a big discussion with Dan on facebook about this. What it actually could be is the absorbing element of ∨ (that is p ∨ 1 = 1) and the identity of element of ∧ (so p ∧ 1 = p). In both of these cases 1 is just ⊤. 

--However a much simpler interpretation is that 1 represents ⊤.


--Then for p4 we are given 2 conjunctions as a disjunction so we give proofs for both. For (p ∧ ¬ q) we get p using and-1 for the left side of the proof and then use or-1 to constuct the right hand side by giving ¬ q. We get ¬ q using and-2 on (p ∧¬ q)

--Then for the right parameter we have (p ∧ ⊤) so  we get p by using and-2 on this and then construct the right hand side using or-2 on ⊤ which we get by using and-2 again on (p ∧ ⊤)
p4 : {p q  : Set} →  (p ∧ ¬ q) ∨ (p ∧ ⊤) → p ∧ (¬ q ∨ ⊤)
p4 {p} {q} (inl x) = and-1 x , or-1 (and-2 x)
p4 {p} {q} (inr x) = (and-1 x , or-2 (and-2 x))
 
  
  --for p5 we are given a disjunction, so we give the parameter (x , y), where x is an element of p and y is an element of (¬ q ∨ ⊤). Then we get the proof by giving p for the left hand side and getting ⊤ using p5-1. p5-1 says that given (¬ q ∨ ⊤), for the left hand side, defining any element is the same as constructing an element of ⊤ using tt (the constructor for ⊤), and for the right hand side we already have x, an element of ⊤ so we just give this:
p5 : {p q : Set} → p ∧ (¬ q ∨ ⊤) → p ∧ ⊤
p5 {p} {q} (x , y) = x , tt

--finally, for p6 we get p from p ∧ ⊤ by just using and-1
p6 : {p  : Set} → p ∧ ⊤ → p
p6 {p}  = and-1

--now we chain them all together on a parameter, p,  of type ((B → A) → B)
peirce : {A B : Set} → ((B → A) → B) → B
peirce {A} {B} p  = p6 (p5 (p4 (p3 (p2 (p1 (p0 p))))))



-- Optional Exercise : Use intuitionistic logic to prove this version of Peirce's law

{-

Given ((B → A) → B) and ¬B , we need to prove ⊥. We can do this using not-1, where the sets A and B given are now B → A and B respectively. Then the parameters are of type  (B → A) → B, which is given in the defintion of pierce' as p, (B → A ) → ¬ B, which we construct as a subgoal : given a proof p that B → A, we use the b given to prove this to get an element of ¬ B. Then the final parameter is he function B → A (so that ¬ B → A reduces to ⊥). This is given by giving b and using not-2 to show that A follows from B and ¬ B.

-}
peirce' : {A B : Set} → ((B → A) → B) → ¬ (¬ B)
peirce' {A} {B} p nb = not-1 {B → A} {B} p goal1 goal2 
    where
      goal1 : (B → A) → ¬ B
      goal1 p b = nb b
      goal2 : B → A
      goal2 b = not-2 b nb

