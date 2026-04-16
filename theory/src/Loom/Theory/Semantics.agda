{-# OPTIONS --safe #-}

module Loom.Theory.Semantics where

open import Data.Fin.Properties as Finₚ using (_≟_)
open import Data.Nat.Properties as ℕₚ using (_≟_)
open import Loom.Theory.Access
open import Loom.Theory.Index
open import Loom.Theory.Prelude
open import Loom.Theory.Schedule
open import Loom.Theory.Shape
open import Loom.Theory.Syntax
open import Relation.Nullary using (Dec; no; yes)

Store : ∀ {rank} → Shape rank → Set
Store shape = RectIx shape → ℕ

Env : Rank → Set
Env rank = (arr : Array rank) → Store (shape arr)

emptyEnv : ∀ {rank} → Env rank
emptyEnv _ _ = zero

lookupEnv : ∀ {rank} → (env : Env rank) → (arr : Array rank) → RectIx (shape arr) → ℕ
lookupEnv env arr ix = env arr ix

PostStateEq : ∀ {rank} → Env rank → Env rank → Set
PostStateEq {rank} left right =
  (arr : Array rank) →
  (ix : RectIx (shape arr)) →
  lookupEnv left arr ix ≡ lookupEnv right arr ix

rectIxEq : ∀ {rank} {shape : Shape rank} → (left right : RectIx shape) → Dec (left ≡ right)
rectIxEq {rank1} {shape1 _} left right = left Finₚ.≟ right
rectIxEq {rank2} {shape2 _ _} (leftRow , leftCol) (rightRow , rightCol) with leftRow Finₚ.≟ rightRow | leftCol Finₚ.≟ rightCol
... | yes refl | yes refl = yes refl
... | no left≢right | _ = no λ where refl → left≢right refl
... | _ | no left≢right = no λ where refl → left≢right refl

resolve : ∀ {cap rank sched arr} → Access {rank} cap sched arr → RectIx (shape arr)
resolve {sched = rect} access = ix access
resolve {sched = tile _} access = global (ix access)

readStore : ∀ {cap rank sched arr} → Store (shape arr) → Access {rank} cap sched arr → ℕ
readStore store access = store (resolve access)

writeRect : ∀ {rank} {shape : Shape rank} → Store shape → RectIx shape → ℕ → Store shape
writeRect store source value target with rectIxEq target source
... | yes _ = value
... | no _ = store target

writeStore :
  ∀ {cap rank sched arr} →
  Store (shape arr) →
  Access {rank} cap sched arr →
  ℕ →
  Store (shape arr)
writeStore store access value = writeRect store (resolve access) value

writeRect-self :
  ∀ {rank} {shape : Shape rank} →
  (store : Store shape) →
  (source : RectIx shape) →
  (value : ℕ) →
  writeRect store source value source ≡ value
writeRect-self store source value with rectIxEq source source
... | yes refl = refl
... | no impossible = ⊥-elim (impossible refl)

writeRect-other :
  ∀ {rank} {shape : Shape rank} →
  (store : Store shape) →
  (source target : RectIx shape) →
  (value : ℕ) →
  source ≢ target →
  writeRect store source value target ≡ store target
writeRect-other store source target value source≢target with rectIxEq target source
... | yes refl = ⊥-elim (source≢target refl)
... | no _ = refl

updateEnv :
  ∀ {cap rank sched} →
  (env : Env rank) →
  (arr : Array rank) →
  Access {rank} cap sched arr →
  ℕ →
  Env rank
updateEnv env source access value target with arrayEq source target
... | yes refl = writeStore (env source) access value
... | no _ = env target

updateEnv-same :
  ∀ {cap rank sched} →
  (env : Env rank) →
  (arr : Array rank) →
  (access : Access {rank} cap sched arr) →
  (value : ℕ) →
  lookupEnv (updateEnv env arr access value) arr (resolve access) ≡ value
updateEnv-same env arr access value with arrayEq arr arr
... | yes refl = writeRect-self (env arr) (resolve access) value
... | no impossible = ⊥-elim (impossible refl)

updateEnv-other :
  ∀ {cap rank sched} →
  (env : Env rank) →
  (source target : Array rank) →
  (access : Access {rank} cap sched source) →
  (value : ℕ) →
  source ≢ target →
  (ix : RectIx (shape target)) →
  lookupEnv (updateEnv env source access value) target ix ≡ lookupEnv env target ix
updateEnv-other env source target access value source≢target ix with arrayEq source target
... | yes refl = ⊥-elim (source≢target refl)
... | no _ = refl

updateEnv-other-index :
  ∀ {cap rank sched} →
  (env : Env rank) →
  (arr : Array rank) →
  (access : Access {rank} cap sched arr) →
  (value : ℕ) →
  (ix : RectIx (shape arr)) →
  resolve access ≢ ix →
  lookupEnv (updateEnv env arr access value) arr ix ≡ lookupEnv env arr ix
updateEnv-other-index env arr access value ix access≢ix with arrayEq arr arr
... | yes refl = writeRect-other (env arr) (resolve access) ix value access≢ix
... | no impossible = ⊥-elim (impossible refl)

evalKernel : ∀ {rank sched ty} → Env rank → Kernel {rank} sched ty → Env rank × ⟦ ty ⟧Ty
evalKernel env (pure value) = env , value
evalKernel env (kernel >>= next) with evalKernel env kernel
... | env′ , value = evalKernel env′ (next value)
evalKernel env (read {arr = arr} _ access next) =
  evalKernel env (next (readStore (env arr) access))
evalKernel env (write {arr = arr} _ access value) =
  updateEnv env arr access value , tt

runAt :
  ∀ {rank sched shape} →
  Env rank →
  Program {rank} sched shape →
  Index sched shape →
  Env rank
runAt env prog ix = proj₁ (evalKernel env (body prog ix))

evalKernel-pure :
  ∀ {rank sched ty} →
  (env : Env rank) →
  (value : ⟦ ty ⟧Ty) →
  evalKernel {rank} {sched} env (pure value) ≡ (env , value)
evalKernel-pure env value = refl

evalKernel-write :
  ∀ {cap rank sched arr} →
  (env : Env rank) →
  (writable : CanWrite cap) →
  (access : Access {rank} cap sched arr) →
  (value : ℕ) →
  evalKernel env (write writable access value) ≡ (updateEnv env arr access value , tt)
evalKernel-write env writable access value = refl

runAt-body :
  ∀ {rank sched shape} →
  (env : Env rank) →
  (prog : Program {rank} sched shape) →
  (ix : Index sched shape) →
  runAt env prog ix ≡ proj₁ (evalKernel env (body prog ix))
runAt-body env prog ix = refl
