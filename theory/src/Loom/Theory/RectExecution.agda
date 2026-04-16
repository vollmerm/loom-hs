{-# OPTIONS --safe #-}

module Loom.Theory.RectExecution where

open import Loom.Theory.Index
open import Loom.Theory.Prelude
open import Loom.Theory.Schedule
open import Loom.Theory.Semantics
open import Loom.Theory.Shape
open import Loom.Theory.Syntax
open import Loom.Theory.Traversal using (foldFin; runEnumerator; rowMajorTraversal2D; columnMajorTraversal2D)

runSeq :
  ∀ {n rank sched shape} →
  Env rank →
  Program {rank} sched shape →
  (Fin n → Index sched shape) →
  Env rank
runSeq env prog steps = foldFin (λ i env′ → runAt env′ prog (steps i)) env

runRow :
  ∀ {rows cols} →
  Env rank2 →
  Program rect (shape2 rows cols) →
  Fin rows →
  Env rank2
runRow {rows} {cols} env prog row =
  foldFin (λ col env′ → runAt env′ prog (row , col)) env

runCol :
  ∀ {rows cols} →
  Env rank2 →
  Program rect (shape2 rows cols) →
  Fin cols →
  Env rank2
runCol {rows} {cols} env prog col =
  foldFin (λ row env′ → runAt env′ prog (row , col)) env

runRect1 :
  ∀ {shape : Shape rank1} →
  Env rank1 →
  Program rect shape →
  Env rank1
runRect1 {shape1 _} env prog = foldFin (λ ix env′ → runAt env′ prog ix) env

runRect2 :
  ∀ {shape : Shape rank2} →
  Env rank2 →
  Program rect shape →
  Env rank2
runRect2 {shape2 _ _} env prog =
  runEnumerator env prog rowMajorTraversal2D

runRect2Interchange :
  ∀ {shape : Shape rank2} →
  Env rank2 →
  Program rect shape →
  Env rank2
runRect2Interchange {shape2 _ _} env prog =
  runEnumerator env prog columnMajorTraversal2D
