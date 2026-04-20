{-# OPTIONS --safe #-}

module Loom.Theory.WholeTiled where

open import Loom.Theory.Access
open import Loom.Theory.Index
open import Loom.Theory.Pointwise
open import Loom.Theory.Prelude
open import Loom.Theory.RectExecution
open import Loom.Theory.Schedule
open import Loom.Theory.Semantics
open import Loom.Theory.Shape
open import Loom.Theory.Syntax
open import Loom.Theory.Traversal using (Enumerator; LinearTraversal; runEnumerator; stepAt; traversal)
open import Loom.Theory.TiledPointwise
open import Loom.Theory.WholeLinear using (WholeKernel)

record WholeTiledKernel (n : ℕ) (dom tileShape : Shape rank2) : Set where
  field
    baseKernel : TiledPointwiseKernel dom tileShape
    steps : LinearTraversal {A = TileIx dom tileShape} n
    outputUnique :
      ∀ {i j} →
      outputGlobal baseKernel (stepAt steps i) ≡ outputGlobal baseKernel (stepAt steps j) →
      i ≡ j

open WholeTiledKernel public

tileAt :
  ∀ {n dom tileShape} →
  WholeTiledKernel n dom tileShape →
  Fin n →
  TileIx dom tileShape
tileAt kernel = stepAt (steps kernel)

tileTraversal :
  ∀ {n dom tileShape} →
  WholeTiledKernel n dom tileShape →
  Enumerator (TileIx dom tileShape)
tileTraversal kernel = traversal (steps kernel)

tiledProgram :
  ∀ {n dom tileShape} →
  WholeTiledKernel n dom tileShape →
  Program (tile tileShape) dom
tiledProgram kernel = kernelProgram (base (baseKernel kernel))

runTiles :
  ∀ {n dom tileShape} →
  Env rank2 →
  WholeTiledKernel n dom tileShape →
  Env rank2
runTiles env kernel = runEnumerator env (tiledProgram kernel) (tileTraversal kernel)

fsuc-injective : ∀ {n} {i j : Fin n} → fsuc i ≡ fsuc j → i ≡ j
fsuc-injective refl = refl

fsuc≢fzero : ∀ {n} {i : Fin n} → fsuc i ≢ fzero
fsuc≢fzero ()

tailKernel :
  ∀ {n dom tileShape} →
  WholeTiledKernel (suc n) dom tileShape →
  WholeTiledKernel n dom tileShape
tailKernel kernel = record
  { baseKernel = baseKernel kernel
  ; steps = Loom.Theory.Traversal.linearTraversal (λ i → tileAt kernel (fsuc i))
  ; outputUnique = λ eq → fsuc-injective (outputUnique kernel eq)
  }

runTiles-step :
  ∀ {n dom tileShape} →
  (kernel : WholeTiledKernel (suc n) dom tileShape) →
  (env : Env rank2) →
  runTiles env kernel ≡
    runTiles
      (runAt env (tiledProgram kernel) (tileAt kernel fzero))
      (tailKernel kernel)
runTiles-step kernel env = refl

runTiles-unrelated :
  ∀ {n dom tileShape} →
  (kernel : WholeTiledKernel n dom tileShape) →
  (env : Env rank2) →
  (other : Array rank2) →
  outputArr (baseKernel kernel) ≢ other →
  (j : RectIx (shape other)) →
  lookupEnv (runTiles env kernel) other j ≡ lookupEnv env other j
runTiles-unrelated {zero} kernel env other output≢other j = refl
runTiles-unrelated {suc n} kernel env other output≢other j
  rewrite runTiles-step kernel env =
    trans
      (runTiles-unrelated
        (tailKernel kernel)
        (runAt env (tiledProgram kernel) (tileAt kernel fzero))
        other
        output≢other
        j)
      (runAt-tiled-unrelated
        (baseKernel kernel)
        env
        (tileAt kernel fzero)
        other
        output≢other
        j)

runTiles-preserve-target :
  ∀ {n dom tileShape} →
  (kernel : WholeTiledKernel n dom tileShape) →
  (env : Env rank2) →
  (target : RectIx (shape (outputArr (baseKernel kernel)))) →
  (noHit : ∀ i → outputGlobal (baseKernel kernel) (tileAt kernel i) ≢ target) →
  lookupEnv (runTiles env kernel) (outputArr (baseKernel kernel)) target ≡
    lookupEnv env (outputArr (baseKernel kernel)) target
runTiles-preserve-target {zero} kernel env target noHit = refl
runTiles-preserve-target {suc n} kernel env target noHit
  rewrite runTiles-step kernel env =
    trans
      (runTiles-preserve-target
        (tailKernel kernel)
        (runAt env (tiledProgram kernel) (tileAt kernel fzero))
        target
        (λ i → noHit (fsuc i)))
      (updateEnv-other-index
        env
        (outputArr (baseKernel kernel))
        (outputAt (baseKernel kernel) (tileAt kernel fzero))
        (transform (baseKernel kernel)
          (lookupEnv env (inputArr (baseKernel kernel))
            (resolve (inputAt (baseKernel kernel) (tileAt kernel fzero)))))
        target
        (λ eq → noHit fzero (trans (sym (output-global (baseKernel kernel) (tileAt kernel fzero))) eq)))

runTiles-pointwise :
  ∀ {n dom tileShape} →
  (kernel : WholeTiledKernel n dom tileShape) →
  (env : Env rank2) →
  (i : Fin n) →
  lookupEnv (runTiles env kernel) (outputArr (baseKernel kernel))
    (outputGlobal (baseKernel kernel) (tileAt kernel i)) ≡
    transform (baseKernel kernel)
      (lookupEnv env (inputArr (baseKernel kernel)) (inputGlobal (baseKernel kernel) (tileAt kernel i)))
runTiles-pointwise {zero} kernel env ()
runTiles-pointwise {suc n} kernel env fzero
  rewrite runTiles-step kernel env =
    trans
      (runTiles-preserve-target
        (tailKernel kernel)
        (runAt env (tiledProgram kernel) (tileAt kernel fzero))
        (outputGlobal (baseKernel kernel) (tileAt kernel fzero))
        (λ i eq → fsuc≢fzero (outputUnique kernel eq)))
      (runAt-tiled-pointwise
        (baseKernel kernel)
        env
        (tileAt kernel fzero))
runTiles-pointwise {suc n} kernel env (fsuc i)
  rewrite runTiles-step kernel env =
    trans
      (runTiles-pointwise
        (tailKernel kernel)
        (runAt env (tiledProgram kernel) (tileAt kernel fzero))
        i)
      (cong
        (transform (baseKernel kernel))
        (runAt-tiled-input-preserved
          (baseKernel kernel)
          env
          (tileAt kernel fzero)
          (inputGlobal (baseKernel kernel) (tileAt kernel (fsuc i)))))

runTiles-input-preserved :
  ∀ {n dom tileShape} →
  (kernel : WholeTiledKernel n dom tileShape) →
  (env : Env rank2) →
  (j : RectIx (shape (inputArr (baseKernel kernel)))) →
  lookupEnv (runTiles env kernel) (inputArr (baseKernel kernel)) j ≡
    lookupEnv env (inputArr (baseKernel kernel)) j
runTiles-input-preserved kernel env j =
  runTiles-unrelated
    kernel
    env
    (inputArr (baseKernel kernel))
    (λ eq → distinct (baseKernel kernel) (sym eq))
    j

-- Convert to the generic WholeLinear.WholeKernel.
-- runTiles env k  =  WholeLinear.runWhole env (toWholeKernel k)  by refl.
toWholeKernel :
  ∀ {n dom tileShape} →
  WholeTiledKernel n dom tileShape →
  WholeKernel {rank2} {tile tileShape} {dom} n
toWholeKernel kernel = record
  { base         = base (baseKernel kernel)
  ; steps        = steps kernel
  ; outputUnique =
      λ {i} {j} eq →
        outputUnique kernel
          (trans
            (sym (output-global (baseKernel kernel) (tileAt kernel i)))
            (trans eq
              (output-global (baseKernel kernel) (tileAt kernel j))))
  }
