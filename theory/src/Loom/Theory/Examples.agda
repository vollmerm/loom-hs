{-# OPTIONS --safe #-}

module Loom.Theory.Examples where

open import Loom.Theory.Access
open import Loom.Theory.ExactCoverRect1
open import Loom.Theory.ExactCoverRect2
open import Loom.Theory.ExactCoverTiled
open import Loom.Theory.Index
open import Loom.Theory.Pointwise
open import Loom.Theory.Phase
open import Loom.Theory.PhaseSemantics
open import Loom.Theory.Prelude
open import Loom.Theory.RectExecution
open import Loom.Theory.Schedule
open import Loom.Theory.Semantics
open import Loom.Theory.Shape
open import Loom.Theory.Syntax
open import Loom.Theory.TiledPointwise
open import Loom.Theory.Traversal using (linearTraversal)
open import Loom.Theory.WholeTiled
open import Loom.Theory.WholeRect1
open import Loom.Theory.WholeRect2
open import Relation.Nullary using (Dec; no; yes)

board : Shape rank2
board = shape2 8 8

tile4x4 : Shape rank2
tile4x4 = shape2 4 4

tile1x2 : Shape rank2
tile1x2 = shape2 1 2

input : Array rank2
input = array 0 board

output : Array rank2
output = array 1 board

rect-copy : Program rect board
rect-copy = program λ ix →
  read read-only (rectAccess input ix) λ value →
    write write-only (rectAccess output ix) value

rect-copy-kernel : PointwiseKernel rect board
rect-copy-kernel = record
  { inputArr = input
  ; outputArr = output
  ; distinct = λ ()
  ; inputAt = rectAccess input
  ; outputAt = rectAccess output
  ; transform = λ value → value
  }

rect-inc : Program rect board
rect-inc = program λ ix →
  read read-only (rectAccess input ix) λ value →
    write write-only (rectAccess output ix) (suc value)

rect-inc-kernel : PointwiseKernel rect board
rect-inc-kernel = record
  { inputArr = input
  ; outputArr = output
  ; distinct = λ ()
  ; inputAt = rectAccess input
  ; outputAt = rectAccess output
  ; transform = suc
  }

tiled-copy : Program (tile tile4x4) board
tiled-copy = program λ ix →
  read read-only (tileAccess input ix) λ value →
    write write-only (tileAccess output ix) value

tiled-copy-kernel : PointwiseKernel (tile tile4x4) board
tiled-copy-kernel = record
  { inputArr = input
  ; outputArr = output
  ; distinct = λ ()
  ; inputAt = tileAccess input
  ; outputAt = tileAccess output
  ; transform = λ value → value
  }

tiled-copy-global-kernel : TiledPointwiseKernel board tile4x4
tiled-copy-global-kernel = record
  { inputArr = input
  ; outputArr = output
  ; distinct = λ ()
  ; input-shape = refl
  ; output-shape = refl
  ; inputAt = tileAccess input
  ; outputAt = tileAccess output
  ; transform = λ value → value
  ; input-global = λ _ → refl
  ; output-global = λ _ → refl
  }

tiled-global : TileIx board tile4x4 → RectIx board
tiled-global = global

lt8 : zero + zero < 8
lt8 = s≤s z≤n

lt4 : zero + zero < 4
lt4 = s≤s z≤n

zero₂ : RectIx board
zero₂ = fromℕ< lt8 , fromℕ< lt8

zero4 : RectIx tile4x4
zero4 = fromℕ< lt4 , fromℕ< lt4

tile-zero : TileIx board tile4x4
tile-zero = tileIx zero₂ zero4 lt8 lt8

constantStore : ∀ {rank} {shape : Shape rank} → ℕ → Store shape
constantStore value _ = value

line : Shape rank1
line = shape1 3

line-input : Array rank1
line-input = array 5 line

line-output : Array rank1
line-output = array 6 line

line-inc-output : Array rank1
line-inc-output = array 7 line

line-rect-copy : Program rect line
line-rect-copy = program λ ix →
  read read-only (rectAccess line-input ix) λ value →
    write write-only (rectAccess line-output ix) value

line-copy-kernel : PointwiseKernel rect line
line-copy-kernel = record
  { inputArr = line-input
  ; outputArr = line-output
  ; distinct = λ ()
  ; inputAt = rectAccess line-input
  ; outputAt = rectAccess line-output
  ; transform = λ value → value
  }

line-rect-inc : Program rect line
line-rect-inc = program λ ix →
  read read-only (rectAccess line-input ix) λ value →
    write write-only (rectAccess line-inc-output ix) (suc value)

line-inc-kernel : PointwiseKernel rect line
line-inc-kernel = record
  { inputArr = line-input
  ; outputArr = line-inc-output
  ; distinct = λ ()
  ; inputAt = rectAccess line-input
  ; outputAt = rectAccess line-inc-output
  ; transform = suc
  }

line-output-inc : Program rect line
line-output-inc = program λ ix →
  read read-only (rectAccess line-output ix) λ value →
    write write-only (rectAccess line-inc-output ix) (suc value)

line-output-inc-kernel : PointwiseKernel rect line
line-output-inc-kernel = record
  { inputArr = line-output
  ; outputArr = line-inc-output
  ; distinct = λ ()
  ; inputAt = rectAccess line-output
  ; outputAt = rectAccess line-inc-output
  ; transform = suc
  }

line-whole-kernel : WholeRect1Kernel 3
line-whole-kernel = record
  { base = line-copy-kernel
  ; outputUnique = λ eq → eq
  }

line-inc-whole-kernel : WholeRect1Kernel 3
line-inc-whole-kernel = record
  { base = line-inc-kernel
  ; outputUnique = λ eq → eq
  }

line-output-inc-whole-kernel : WholeRect1Kernel 3
line-output-inc-whole-kernel = record
  { base = line-output-inc-kernel
  ; outputUnique = λ eq → eq
  }

line-copy-then-inc : PhasedProgram rank1
line-copy-then-inc = step line-rect-copy (singlePhase line-output-inc)

line-0 : RectIx line
line-0 = fzero

line-1 : RectIx line
line-1 = fsuc fzero

line-2 : RectIx line
line-2 = fsuc (fsuc fzero)

line-initial : Env rank1
line-initial arr = inputCase (arrayEq arr line-input)
  where
    inputCase : Dec (arr ≡ line-input) → Store (shape arr)
    inputCase (yes refl) = constantStore 4
    inputCase (no _) = constantStore 0

line-copy-zero :
  lookupEnv (runAt line-initial line-rect-copy line-0) line-output line-0 ≡ 4
line-copy-zero = refl

line-inc-zero :
  lookupEnv (runAt line-initial line-rect-inc line-0) line-inc-output line-0 ≡ 5
line-inc-zero = refl

line-copy-then-inc-zero :
  lookupEnv (runPhases line-initial line-copy-then-inc) line-inc-output line-0 ≡ 5
line-copy-then-inc-zero = refl

initial₂ : Env rank2
initial₂ arr = inputCase (arrayEq arr input)
  where
    inputCase : Dec (arr ≡ input) → Store (shape arr)
    inputCase (yes refl) = constantStore 7
    inputCase (no _) = constantStore 0

rect-copy-zero :
  lookupEnv (runAt initial₂ rect-copy zero₂) output zero₂ ≡ 7
rect-copy-zero = refl

tiled-copy-zero :
  lookupEnv (runAt initial₂ tiled-copy tile-zero) output zero₂ ≡ 7
tiled-copy-zero = refl

tiny-board : Shape rank2
tiny-board = shape2 2 2

tiny-input : Array rank2
tiny-input = array 2 tiny-board

tiny-output : Array rank2
tiny-output = array 3 tiny-board

tiny-inc-output : Array rank2
tiny-inc-output = array 4 tiny-board

tiny-rect-copy : Program rect tiny-board
tiny-rect-copy = program λ ix →
  read read-only (rectAccess tiny-input ix) λ value →
    write write-only (rectAccess tiny-output ix) value

tiny-rect-copy-kernel : PointwiseKernel rect tiny-board
tiny-rect-copy-kernel = record
  { inputArr = tiny-input
  ; outputArr = tiny-output
  ; distinct = λ ()
  ; inputAt = rectAccess tiny-input
  ; outputAt = rectAccess tiny-output
  ; transform = λ value → value
  }

tiny-copy-whole-kernel : WholeRect2Kernel 2 2
tiny-copy-whole-kernel = record
  { base = tiny-rect-copy-kernel
  ; outputUnique = λ eq → eq
  }

line-exact-kernel : ExactCoverRect1Kernel 3
line-exact-kernel = record
  { wholeKernel = line-whole-kernel
  ; coverIndex = λ target → target
  ; outputCovered = λ target → refl
  }

line-inc-exact-kernel : ExactCoverRect1Kernel 3
line-inc-exact-kernel = record
  { wholeKernel = line-inc-whole-kernel
  ; coverIndex = λ target → target
  ; outputCovered = λ target → refl
  }

tiny-rect-inc : Program rect tiny-board
tiny-rect-inc = program λ ix →
  read read-only (rectAccess tiny-input ix) λ value →
    write write-only (rectAccess tiny-inc-output ix) (suc value)

tiny-rect-inc-kernel : PointwiseKernel rect tiny-board
tiny-rect-inc-kernel = record
  { inputArr = tiny-input
  ; outputArr = tiny-inc-output
  ; distinct = λ ()
  ; inputAt = rectAccess tiny-input
  ; outputAt = rectAccess tiny-inc-output
  ; transform = suc
  }

tiny-inc-whole-kernel : WholeRect2Kernel 2 2
tiny-inc-whole-kernel = record
  { base = tiny-rect-inc-kernel
  ; outputUnique = λ eq → eq
  }

tiny-copy-exact-kernel : ExactCoverRect2Kernel 2 2
tiny-copy-exact-kernel = record
  { wholeKernel = tiny-copy-whole-kernel
  ; coverRow = proj₁
  ; coverCol = proj₂
  ; outputCovered = λ target → refl
  }

tiny-inc-exact-kernel : ExactCoverRect2Kernel 2 2
tiny-inc-exact-kernel = record
  { wholeKernel = tiny-inc-whole-kernel
  ; coverRow = proj₁
  ; coverCol = proj₂
  ; outputCovered = λ target → refl
  }

lt2 : zero + zero < 2
lt2 = s≤s z≤n

lt1-2 : suc zero < 2
lt1-2 = s≤s (s≤s z≤n)

tiny-zero₂ : RectIx tiny-board
tiny-zero₂ = fromℕ< lt2 , fromℕ< lt2

tiny-tile-row0 : RectIx tile1x2
tiny-tile-row0 = fzero , fzero

tiny-tile-row1 : RectIx tile1x2
tiny-tile-row1 = fzero , fsuc fzero

tiny-00 : RectIx tiny-board
tiny-00 = fromℕ< lt2 , fromℕ< lt2

tiny-01 : RectIx tiny-board
tiny-01 = fromℕ< lt2 , fromℕ< lt1-2

tiny-10 : RectIx tiny-board
tiny-10 = fromℕ< lt1-2 , fromℕ< lt2

tiny-11 : RectIx tiny-board
tiny-11 = fromℕ< lt1-2 , fromℕ< lt1-2

tiny-stripmine-copy : Program (tile tile1x2) tiny-board
tiny-stripmine-copy = program λ ix →
  read read-only (tileAccess tiny-input ix) λ value →
    write write-only (tileAccess tiny-output ix) value

tiny-stripmine-copy-kernel : TiledPointwiseKernel tiny-board tile1x2
tiny-stripmine-copy-kernel = record
  { inputArr = tiny-input
  ; outputArr = tiny-output
  ; distinct = λ ()
  ; input-shape = refl
  ; output-shape = refl
  ; inputAt = tileAccess tiny-input
  ; outputAt = tileAccess tiny-output
  ; transform = λ value → value
  ; input-global = λ _ → refl
  ; output-global = λ _ → refl
  }

tiny-stripmine-step-0 : TileIx tiny-board tile1x2
tiny-stripmine-step-0 = tileIx tiny-00 tiny-tile-row0 lt2 lt2

tiny-stripmine-step-1 : TileIx tiny-board tile1x2
tiny-stripmine-step-1 = tileIx tiny-00 tiny-tile-row1 lt2 lt1-2

tiny-stripmine-step-2 : TileIx tiny-board tile1x2
tiny-stripmine-step-2 = tileIx tiny-10 tiny-tile-row0 lt1-2 lt2

tiny-stripmine-step-3 : TileIx tiny-board tile1x2
tiny-stripmine-step-3 = tileIx tiny-10 tiny-tile-row1 lt1-2 lt1-2

tiny-stripmine-at : Fin 4 → TileIx tiny-board tile1x2
tiny-stripmine-at fzero = tiny-stripmine-step-0
tiny-stripmine-at (fsuc fzero) = tiny-stripmine-step-1
tiny-stripmine-at (fsuc (fsuc fzero)) = tiny-stripmine-step-2
tiny-stripmine-at (fsuc (fsuc (fsuc fzero))) = tiny-stripmine-step-3

tiny-stripmine-whole-kernel : WholeTiledKernel 4 tiny-board tile1x2
tiny-stripmine-whole-kernel = record
  { baseKernel = tiny-stripmine-copy-kernel
  ; steps = linearTraversal tiny-stripmine-at
  ; outputUnique = λ eq → outputUniqueCase eq
  }
  where
    outputUniqueCase :
      ∀ {i j} →
      outputGlobal tiny-stripmine-copy-kernel (tiny-stripmine-at i) ≡
      outputGlobal tiny-stripmine-copy-kernel (tiny-stripmine-at j) →
      i ≡ j
    outputUniqueCase {fzero} {fzero} eq = refl
    outputUniqueCase {fzero} {fsuc fzero} ()
    outputUniqueCase {fzero} {fsuc (fsuc fzero)} ()
    outputUniqueCase {fzero} {fsuc (fsuc (fsuc fzero))} ()
    outputUniqueCase {fsuc fzero} {fzero} ()
    outputUniqueCase {fsuc fzero} {fsuc fzero} eq = refl
    outputUniqueCase {fsuc fzero} {fsuc (fsuc fzero)} ()
    outputUniqueCase {fsuc fzero} {fsuc (fsuc (fsuc fzero))} ()
    outputUniqueCase {fsuc (fsuc fzero)} {fzero} ()
    outputUniqueCase {fsuc (fsuc fzero)} {fsuc fzero} ()
    outputUniqueCase {fsuc (fsuc fzero)} {fsuc (fsuc fzero)} eq = refl
    outputUniqueCase {fsuc (fsuc fzero)} {fsuc (fsuc (fsuc fzero))} ()
    outputUniqueCase {fsuc (fsuc (fsuc fzero))} {fzero} ()
    outputUniqueCase {fsuc (fsuc (fsuc fzero))} {fsuc fzero} ()
    outputUniqueCase {fsuc (fsuc (fsuc fzero))} {fsuc (fsuc fzero)} ()
    outputUniqueCase {fsuc (fsuc (fsuc fzero))} {fsuc (fsuc (fsuc fzero))} eq = refl

tiny-stripmine-exact-kernel : ExactCoverTiledKernel 4 tiny-board tile1x2
tiny-stripmine-exact-kernel = record
  { wholeKernel = tiny-stripmine-whole-kernel
  ; coverIndex = λ where
      (fzero , fzero) → fzero
      (fzero , fsuc fzero) → fsuc fzero
      (fsuc fzero , fzero) → fsuc (fsuc fzero)
      (fsuc fzero , fsuc fzero) → fsuc (fsuc (fsuc fzero))
  ; outputCovered = λ where
      (fzero , fzero) → refl
      (fzero , fsuc fzero) → refl
      (fsuc fzero , fzero) → refl
      (fsuc fzero , fsuc fzero) → refl
  }

tiny-initial₂ : Env rank2
tiny-initial₂ arr = inputCase (arrayEq arr tiny-input)
  where
    inputCase : Dec (arr ≡ tiny-input) → Store (shape arr)
    inputCase (yes refl) = constantStore 5
    inputCase (no _) = constantStore 0

tiny-rect-copy-full-zero :
  lookupEnv (runRect2 tiny-initial₂ tiny-rect-copy) tiny-output tiny-zero₂ ≡ 5
tiny-rect-copy-full-zero = refl

line-rect-copy-full-zero :
  lookupEnv (runRect1 line-initial line-rect-copy) line-output line-0 ≡ 4
line-rect-copy-full-zero = refl
