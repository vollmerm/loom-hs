{-# OPTIONS --safe #-}

module Loom.Theory.ExactCoverRect2 where

open import Data.Fin.Base using (combine; remQuot)
open import Data.Fin.Properties using (remQuot-combine)
open import Data.Nat.Base using (_*_)
open import Loom.Theory.Access
open import Loom.Theory.Index
open import Loom.Theory.Pointwise
open import Loom.Theory.Prelude
open import Loom.Theory.RectExecution
open import Loom.Theory.Schedule
open import Loom.Theory.Semantics
open import Loom.Theory.Shape
open import Loom.Theory.WholeRect2
open import Relation.Nullary using (no; yes)
import Loom.Theory.ExactCoverLinear as ExactCoverLinear

record ExactCoverRect2Kernel (rows cols : ℕ) : Set where
  field
    wholeKernel : WholeRect2Kernel rows cols
    coverRow :
      RectIx (shape (outputArr (base wholeKernel))) → Fin rows
    coverCol :
      (target : RectIx (shape (outputArr (base wholeKernel)))) → Fin cols
    outputCovered :
      (target : RectIx (shape (outputArr (base wholeKernel)))) →
      resolve (outputAt (base wholeKernel) (coverRow target , coverCol target)) ≡ target

open ExactCoverRect2Kernel public

runRect2-covered-pointwise :
  ∀ {rows cols} →
  (kernel : ExactCoverRect2Kernel rows cols) →
  (env : Env rank2) →
  (target : RectIx (shape (outputArr (base (wholeKernel kernel))))) →
  lookupEnv (runRect2 env (kernelProgram (base (wholeKernel kernel))))
    (outputArr (base (wholeKernel kernel)))
    target ≡
    transform (base (wholeKernel kernel))
      (lookupEnv env (inputArr (base (wholeKernel kernel)))
        (resolve
          (inputAt (base (wholeKernel kernel))
            (coverRow kernel target , coverCol kernel target))))
runRect2-covered-pointwise kernel env target =
  trans
    (cong
      (lookupEnv (runRect2 env (kernelProgram (base (wholeKernel kernel))))
        (outputArr (base (wholeKernel kernel))))
      (sym (outputCovered kernel target)))
    (runRect2-pointwise
      (wholeKernel kernel)
      env
      (coverRow kernel target)
      (coverCol kernel target))

rect2Expected :
  ∀ {rows cols} →
  (kernel : ExactCoverRect2Kernel rows cols) →
  (env : Env rank2) →
  (arr : Array rank2) →
  RectIx (shape arr) →
  ℕ
rect2Expected kernel env arr ix with arrayEq arr (outputArr (base (wholeKernel kernel)))
... | yes refl =
  transform (base (wholeKernel kernel))
    (lookupEnv env (inputArr (base (wholeKernel kernel)))
      (resolve
        (inputAt (base (wholeKernel kernel))
          (coverRow kernel ix , coverCol kernel ix))))
... | no _ = lookupEnv env arr ix

runRect2-state :
  ∀ {rows cols} →
  (kernel : ExactCoverRect2Kernel rows cols) →
  (env : Env rank2) →
  (arr : Array rank2) →
  (ix : RectIx (shape arr)) →
  lookupEnv (runRect2 env (kernelProgram (base (wholeKernel kernel)))) arr ix ≡
    rect2Expected kernel env arr ix
runRect2-state kernel env arr ix with arrayEq arr (outputArr (base (wholeKernel kernel)))
... | yes refl = runRect2-covered-pointwise kernel env ix
... | no arr≢output =
  runRect2-unrelated
    (wholeKernel kernel)
    env
    arr
    (λ eq → arr≢output (sym eq))
    ix

runRect2-state-eq :
  ∀ {rows cols} →
  (kernel : ExactCoverRect2Kernel rows cols) →
  (env : Env rank2) →
  PostStateEq
    (runRect2 env (kernelProgram (base (wholeKernel kernel))))
    (rect2Expected kernel env)
runRect2-state-eq kernel env arr ix = runRect2-state kernel env arr ix

-- Bridge: every ExactCoverRect2Kernel is an instance of the generic ExactCoverLinear framework.
-- coverIndex flattens the (row, col) cover witness to a flat Fin (rows * cols) index.
toExactCoverKernel :
  ∀ {rows cols} →
  ExactCoverRect2Kernel rows cols →
  ExactCoverLinear.ExactCoverKernel {rank2} {rect} {shape2 rows cols} (rows * cols)
toExactCoverKernel {rows} {cols} kernel = record
  { wholeKernel  = toWholeKernel (wholeKernel kernel)
  ; coverIndex   = λ target → combine (coverRow kernel target) (coverCol kernel target)
  ; outputCovered = λ target →
      trans
        (cong
          (λ p → resolve (outputAt (base (wholeKernel kernel)) p))
          (remQuot-combine {n = rows} {k = cols} (coverRow kernel target) (coverCol kernel target)))
        (outputCovered kernel target)
  }
