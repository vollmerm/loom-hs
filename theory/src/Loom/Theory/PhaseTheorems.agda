{-# OPTIONS --safe #-}

module Loom.Theory.PhaseTheorems where

open import Loom.Theory.Access
open import Loom.Theory.Index
open import Loom.Theory.Phase
open import Loom.Theory.PhaseSemantics
open import Loom.Theory.Prelude
open import Loom.Theory.RectExecution
open import Loom.Theory.Schedule
open import Loom.Theory.Semantics
open import Loom.Theory.Shape
open import Loom.Theory.Syntax
open import Loom.Theory.Traversal using (Enumerator; elemAt; foldFin; runEnumerator)
open import Relation.Nullary using (no; yes)

foldEnv-cong :
  ∀ {rank n} →
  (stepFn : Fin n → Env rank → Env rank) →
  ((ix : Fin n) →
   (left right : Env rank) →
   PostStateEq left right →
   PostStateEq (stepFn ix left) (stepFn ix right)) →
  (left right : Env rank) →
  PostStateEq left right →
  PostStateEq (foldFin stepFn left) (foldFin stepFn right)
foldEnv-cong {n = zero} stepFn stepCong left right envEq = envEq
foldEnv-cong {n = suc n} stepFn stepCong left right envEq =
  foldEnv-cong
    (λ ix → stepFn (fsuc ix))
    (λ ix → stepCong (fsuc ix))
    (stepFn fzero left)
    (stepFn fzero right)
    (stepCong fzero left right envEq)

updateEnv-cong :
  ∀ {cap rank sched} →
  (left right : Env rank) →
  PostStateEq left right →
  (arr : Array rank) →
  (access : Access {rank} cap sched arr) →
  (value : ℕ) →
  PostStateEq (updateEnv left arr access value) (updateEnv right arr access value)
updateEnv-cong left right envEq arr access value target ix with arrayEq arr target
updateEnv-cong left right envEq arr access value .arr ix | yes refl with rectIxEq ix (resolve access)
updateEnv-cong left right envEq arr access value .arr .(resolve access) | yes refl | yes refl = refl
updateEnv-cong left right envEq arr access value .arr ix | yes refl | no access≢ix = envEq arr ix
updateEnv-cong left right envEq arr access value target ix | no arr≢target = envEq target ix

evalKernel-cong :
  ∀ {rank sched ty} →
  (left right : Env rank) →
  PostStateEq left right →
  (kernel : Kernel {rank} sched ty) →
  PostStateEq (proj₁ (evalKernel left kernel)) (proj₁ (evalKernel right kernel)) ×
  (proj₂ (evalKernel left kernel) ≡ proj₂ (evalKernel right kernel))
evalKernel-cong left right envEq (pure value) = envEq , refl
evalKernel-cong left right envEq (kernel >>= next)
  with evalKernel left kernel | evalKernel right kernel | evalKernel-cong left right envEq kernel
... | left′ , valueLeft | right′ , valueRight | envEq′ , valueEq rewrite valueEq =
  evalKernel-cong left′ right′ envEq′ (next valueRight)
evalKernel-cong left right envEq (read {arr = arr} readable access next)
  rewrite envEq arr (resolve access) =
    evalKernel-cong left right envEq (next (readStore (right arr) access))
evalKernel-cong left right envEq (write {arr = arr} writable access value) =
  updateEnv-cong left right envEq arr access value , refl

runAt-cong :
  ∀ {rank sched shape} →
  (left right : Env rank) →
  PostStateEq left right →
  (prog : Program {rank} sched shape) →
  (ix : Index sched shape) →
  PostStateEq (runAt left prog ix) (runAt right prog ix)
runAt-cong left right envEq prog ix = proj₁ (evalKernel-cong left right envEq (body prog ix))

runEnumerator-cong :
  ∀ {rank sched shape} →
  (left right : Env rank) →
  PostStateEq left right →
  (prog : Program {rank} sched shape) →
  (enumeration : Enumerator (Index sched shape)) →
  PostStateEq
    (runEnumerator left prog enumeration)
    (runEnumerator right prog enumeration)
runEnumerator-cong left right envEq prog enumeration =
  foldEnv-cong
    (λ outer env →
      foldFin
        (λ inner env′ → runAt env′ prog (elemAt enumeration outer inner))
        env)
    outerCong
    left
    right
    envEq
  where
    outerCong :
      (outer : Fin (Enumerator.outerCount enumeration)) →
      (left′ right′ : Env _) →
      PostStateEq left′ right′ →
      PostStateEq
        (foldFin
          (λ inner env → runAt env prog (elemAt enumeration outer inner))
          left′)
        (foldFin
          (λ inner env → runAt env prog (elemAt enumeration outer inner))
          right′)
    outerCong outer left′ right′ envEq′ =
      foldEnv-cong
        (λ inner env → runAt env prog (elemAt enumeration outer inner))
        (λ inner env₁ env₂ eq → runAt-cong env₁ env₂ eq prog (elemAt enumeration outer inner))
        left′
        right′
        envEq′

runRectProgram-cong :
  ∀ {rank} {shape : Shape rank} →
  (left right : Env rank) →
  PostStateEq left right →
  (prog : Program rect shape) →
  PostStateEq (runRectProgram left prog) (runRectProgram right prog)
runRectProgram-cong {rank1} {shape1 _} left right envEq prog =
  foldEnv-cong
    (λ ix env → runAt env prog ix)
    (λ ix env₁ env₂ eq → runAt-cong env₁ env₂ eq prog ix)
    left
    right
    envEq
runRectProgram-cong {rank2} {shape2 _ _} left right envEq prog =
  runEnumerator-cong left right envEq prog rowMajorTraversal2D
  where
    open import Loom.Theory.Traversal using (rowMajorTraversal2D)

runPhases-cong :
  ∀ {rank} →
  (left right : Env rank) →
  PostStateEq left right →
  (phases : PhasedProgram rank) →
  PostStateEq (runPhases left phases) (runPhases right phases)
runPhases-cong left right envEq done = envEq
runPhases-cong left right envEq (step prog phases) =
  runPhases-cong
    (runRectProgram left prog)
    (runRectProgram right prog)
    (runRectProgram-cong left right envEq prog)
    phases
