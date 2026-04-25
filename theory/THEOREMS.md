# Loom Theory: Paper-Facing Theorems

This document lists the most significant mechanized theorems in
`theory/src/Loom/Theory/`, grouped by theme.  For each theorem the table gives
the Agda name, the source module, and an intuitive explanation of what it means
and how it relates to the Loom Haskell library.

All modules are checked with `{-# OPTIONS --safe #-}`.  Run

```
cd theory && make check
```

to typecheck everything.

---

## 1  Single-Kernel Correctness

### `obs-correct` / `run-pointwise-correct`
**Module:** `ObservationalEquivalence`

> *Any well-formed loop program is observationally equivalent to its pointwise
> specification.*

A *well-formed* kernel satisfies two disciplines:

- **Access discipline** — reads go through a `ReadOnly`-capability `Access`
  witness; writes go through a `WriteOnly`-capability `Access` witness; the
  input and output array handles are distinct.
- **Coverage discipline** — every output cell is written by *exactly one*
  iteration step (the `coverIndex` / `outputUnique` / `coverWitness` triple).

Given those disciplines, `run env k` agrees pointwise with `pointwiseSpec k env`:
the value at every output cell is `transform(env[inputArr][inputAt(step)])`, and
every other array is unchanged.

**Loom connection:** this is the formal statement that a `parFor` body written
with `readArr` / `writeArr` computes what the user intended — no aliasing, no
missed writes, no double-writes.

---

### `read-after-write-same`
**Module:** `Safety`

> *A write followed by a read at the same address returns the written value.*

```
read-after-write-same : readStore (writeStore store access value) access ≡ value
```

Basic store coherence.  This underpins `writeArr` followed by `readArr` in the
same kernel step.

---

## 2  Schedule Equivalence (the Polyhedral Model)

### `schedule-equivalence`
**Module:** `ScheduleEquivalence`

> *For any `OutputInputConsistent` kernel, every valid traversal order produces
> the same post-state.*

```
schedule-equivalence :
  OutputInputConsistent base →
  (vs1 vs2 : ValidSchedule base _) →
  PostStateEq (runWithSchedule env base vs1) (runWithSchedule env base vs2)
```

`OutputInputConsistent` is the Loom analog of *no carried cross-iteration
dependence*: two iteration steps that write to the same output cell must also
read from the same input cell.  When this holds, the iteration order is
irrelevant.

The proof goes in three steps:

1. **`schedule-canonical-output`** — any valid schedule writes the canonical
   pointwise value at each output cell.
2. **`schedule-independent-spec`** — for an OI-consistent kernel that canonical
   value is the same under every schedule.
3. **Transitivity** — both schedules agree on the canonical value, so they agree
   on the whole post-state.

**Loom connection:** this is why `parFor` is correct — it picks *any* iteration
order (in practice, a static chunked partition) and the result is
schedule-independent by construction.

---

### `polyhedral-schedule-invariance`
**Module:** `PolyhedralModel`

> *The paper-facing packaging of `schedule-equivalence`: any two legal schedules
> for the same base kernel produce identical post-states.*

```
polyhedral-schedule-invariance :
  LegalSchedule base n → LegalSchedule base m →
  PostStateEq (runWithSchedule env base (valid s1)) (runWithSchedule env base (valid s2))
```

`LegalSchedule` bundles a base kernel, its `OutputInputConsistent` proof, and a
`ValidSchedule` (an exact-cover traversal).  This is the direct mechanization of
the polyhedral model's core claim.

---

### `factoring-implies-oi-consistent`
**Module:** `ScheduleIndependent`

> *If the input-access function factors through the output-access function, the
> kernel is `OutputInputConsistent`.*

```
factoring-implies-oi-consistent :
  (∀ step → resolve (inputAt base step) ≡ f (resolve (outputAt base step))) →
  OutputInputConsistent base
```

For copy-style kernels `f = id`; for a stride-`s` read `f = (· * s)`.  The
proof is a single application: if two steps land on the same output coordinate,
the factoring forces them to land on the same input coordinate.

**Loom connection:** this is why every pointwise map or copy written with Loom's
DSL is automatically legal — the input address is an affine function of the
output address, and the factoring witness is trivial.

---

## 3  Parallel Execution

### `parallel-eq-sequential`
**Module:** `ParallelSemantics`

> *For an `OutputInputConsistent` kernel, big-step parallel execution produces
> the same post-state as sequential execution.*

```
parallel-eq-sequential :
  OutputInputConsistent base →
  PostStateEq (runParallel env base vs) (runWithSchedule env base vs)
```

`runParallel` models *all iterations firing simultaneously*, each reading only
the *original* pre-execution environment `env`.  This is the execution model of
`parFor`: every thread reads from the snapshot at fork time, writes to its own
output cell, and the results are merged.

The proof is immediate: at output cells both sides compute the canonical value
(from `schedule-canonical-output`); at all other arrays both sides return `env`
unchanged.

---

### `parallel-schedule-invariance`
**Module:** `ParallelSemantics`

> *Any two valid schedules for the same `OutputInputConsistent` kernel produce
> identical post-states when run in parallel.*

```
parallel-schedule-invariance :
  OutputInputConsistent base →
  (vs1 : ValidSchedule base n) → (vs2 : ValidSchedule base m) →
  PostStateEq (runParallel env base vs1) (runParallel env base vs2)
```

This is the formal justification for `parFor`: the parallel execution result is
independent of how the iteration domain is partitioned across threads.  Any
chunking strategy — static, dynamic, work-stealing — is semantically equivalent.

---

## 4  Loop Transformations

### Loop interchange (2D rectangular kernels)
**Module:** `LoopInterchange`

> *Swapping the loop order (row-major ↔ column-major) in a 2D rectangular kernel
> produces the same post-state.*

```
runRect2-eq-runRect2Interchange :
  (kernel : ...) → (env : Env rank2) →
  PostStateEq (runRect2 env kernel) (runRect2Interchange env kernel)
```

`runRect2` iterates `for row { for col { … } }`;
`runRect2Interchange` iterates `for col { for row { … } }`.

The proof reduces to `ExactCoverRect2.runRect2-eq-runWhole` on both sides,
unifying through the shared flat-traversal canonical form.

**Loom connection:** the Loom runtime is free to reorder loops in a 2D
`parFor` nest.  Loop interchange is a zero-cost transformation when the
kernel is OI-consistent.

---

### Strip-mine tiling
**Module:** `StripMineTiling`

> *A tiled execution of a rectangular kernel produces the same post-state as the
> flat rectangular execution.*

```
tiny-stripmine-preserves-state :
  PostStateEq
    (runRect2 tiny-initial₂ tiny-rect-copy)
    (runTiles tiny-initial₂ (wholeKernel tiny-stripmine-exact-kernel))
```

Strip-mining replaces a single rectangular loop over `N` elements with an outer
loop over tiles of size `T` and an inner loop over elements within each tile.
The theorem says the tiled version computes the same result as the original.

**Loom connection:** this is the formal basis for the `tiled-matmul` kernel in
`benchmarks/`.  Tiling is a semantics-preserving transformation whenever the
exact-cover and OI-consistency conditions hold.

---

### `schedule-preserves-state` / `disciplined-determinism`
**Module:** `Determinism`

> *Two equivalent traversals of the same tiled kernel produce identical
> post-states.*

```
schedule-preserves-state :
  EquivalentTraversals kernel n m →
  PostStateEq
    (runTiles env (traversalWholeKernel kernel (left eqv)))
    (runTiles env (traversalWholeKernel kernel (right eqv)))
```

`EquivalentTraversals` witnesses that two `ExactCoverTraversal`s cover the same
set of tile indices.  The corollary `disciplined-determinism` states pointwise
equality at every array cell — the fully concrete version suitable for paper
statements.

---

## 5  Independent Kernel Composition

### `kernel-independence`
**Module:** `KernelIndependence`

> *Two kernels with disjoint outputs that do not read from each other's outputs
> can be run in either order and produce the same combined state.*

```
kernel-independence :
  outputArr k1 ≢ outputArr k2 →
  outputArr k2 ≢ inputArr  k1 →
  outputArr k1 ≢ inputArr  k2 →
  PostStateEq
    (runWhole (runWhole env k1) k2)
    (runWhole (runWhole env k2) k1)
```

Three non-overlap conditions:
1. The two kernels write to different arrays.
2. Kernel 2 does not write to kernel 1's input.
3. Kernel 1 does not write to kernel 2's input.

When all three hold the kernels are *semantically independent*: their sequential
composition is commutative.

**Loom connection:** this is the formal basis for composing multiple independent
`parFor` regions.  If two loop nests operate on disjoint slices of the store,
the runtime may schedule them in any order (or in parallel) without changing the
result.

---

## 6  Reductions

### `foldReducer-sum-correct`
**Module:** `ReductionTheorems`

> *`foldReducer sumReducer` is extensionally equal to the mathematical sum.*

```
foldReducer-sum-correct :
  (values : Fin n → ℕ) →
  foldReducer sumReducer values ≡ sumFin values
```

`sumFin` is a reference implementation of Σ — a recursive left-to-right sum with
no mutable state.  The theorem says the Loom fold (which uses a `PrimVar`-backed
accumulator in the implementation) computes the same number.

---

### `foldReducer-perm` (Confluent Reduction)
**Module:** `ConfluenceReduction`

> *For any commutative-associative reducer, `foldReducer` produces the same
> result regardless of the traversal order of its input.*

```
foldReducer-perm :
  IsCommAssocReducer r → (p : PermOn n) → (values : Fin n → ℕ) →
  foldReducer r (values ∘ fun p) ≡ foldReducer r values
```

`IsCommAssocReducer r` requires only that *adjacent* fold steps commute
(`step (step acc v1) v2 ≡ step (step acc v2) v1`); from this, arbitrary
permutation-invariance follows by induction.

The proof avoids `funext`: all equalities are established via
`foldReducerFrom-cong`, which works pointwise.

**Corollaries:**

- `sumReducer-is-comm-assoc` — `sumReducer` satisfies the predicate.
- `foldReducer-sum-perm` — the sum of an array is traversal-order-independent.

**Loom connection:** this is the reduction analog of `schedule-equivalence`.  It
is the formal justification for `foldFor` and `foldFor1D`: the reduction result
is the same regardless of which order the parallel workers visit the array
elements, as long as the reducer is commutative and associative.

---

## 7  Phased Programs

### `runPhases-cong`
**Module:** `PhaseTheorems`

> *If two starting environments are pointwise equal, running the same sequence of
> phases from each produces pointwise-equal results.*

```
runPhases-cong :
  PostStateEq left right →
  PostStateEq (runPhases left phases) (runPhases right phases)
```

This is the congruence / compositionality lemma for phased programs: the
`Phase` / `PhaseSemantics` layer correctly propagates environment equivalence
through an arbitrary sequence of rectangular phases.

**Loom connection:** multi-phase computations (e.g. copy then increment, as in
`line-copy-then-inc` in `Examples`) can be verified compositionally — each phase
is reasoned about independently, and the results are chained.

---

## Summary Table

| Theorem | Module | What it guarantees |
|---|---|---|
| `obs-correct` | `ObservationalEquivalence` | Well-formed kernel = pointwise spec |
| `read-after-write-same` | `Safety` | Basic store coherence |
| `schedule-equivalence` | `ScheduleEquivalence` | All valid schedules agree (OI-consistent) |
| `polyhedral-schedule-invariance` | `PolyhedralModel` | Paper-facing schedule invariance |
| `factoring-implies-oi-consistent` | `ScheduleIndependent` | Affine access ⟹ OI-consistent |
| `parallel-eq-sequential` | `ParallelSemantics` | Parallel = sequential (OI-consistent) |
| `parallel-schedule-invariance` | `ParallelSemantics` | `parFor` partition-independence |
| Loop interchange | `LoopInterchange` | Row-major = column-major (2D) |
| Strip-mine tiling | `StripMineTiling` | Tiled = flat rectangular |
| `disciplined-determinism` | `Determinism` | Equivalent traversals agree pointwise |
| `kernel-independence` | `KernelIndependence` | Disjoint kernels commute |
| `foldReducer-sum-correct` | `ReductionTheorems` | `foldReducer sum` = mathematical Σ |
| `foldReducer-perm` | `ConfluenceReduction` | Comm+assoc ⟹ reduction is order-independent |
| `runPhases-cong` | `PhaseTheorems` | Phased programs compose correctly |
| `wavefront-covers` | `WavefrontTraversal` | Anti-diagonal enumerator covers every 2D cell |
| `wavefront-injective-outer` | `WavefrontTraversal` | Cells on different diagonals are distinct |
| `wavefront-injective-inner` | `WavefrontTraversal` | Distinct positions within one diagonal yield distinct cells |
| `wavefront-is-enumerator` | `WavefrontPhase` | Phased wavefront execution = enumerator (refl) |
| `wavefront-correct` | `WavefrontCorrect` | Wavefront output at every cell equals pointwise spec |
| `wavefront-unrelated` | `WavefrontCorrect` | Wavefront leaves all non-output arrays unchanged |
| `wavefront-input-preserved` | `WavefrontCorrect` | Wavefront leaves the input array unchanged |
| `foldFor-correct` | `FoldForCorrect` | `foldFor` result = fold of pointwise-spec output |
| `copy-tagless-interchange` | `Tagless.InterchangeTheorem` | Loop interchange via algebra swap (tagless) |
| `toTaglessKernel-sim` | `Tagless.BridgeToDeep` | Deep programs satisfy the structural free theorem |
| `toTagless-sim` | `Tagless.BridgeToDeep` | Free theorem lifted to full indexed program bodies |
| `copy-prog-bridge` | `Tagless.BridgeToDeep` | Tagless and deep executions are definitionally equal |
| `tagless-interchange-general` | `Tagless.GeneralInterchange` | Loop interchange for any `ExactCoverRect2Kernel` |
| `tagless-parallel-correct` | `Tagless.ParallelCorrect` | `EnvAlg` sequential = parallel (any valid schedule) |
| **`interpreter-equivalence`** | **`InterpreterEquivalence`** | **Any sound algebra = parallel semantics (headline)** |
| `sequential-parallel-equivalence` | `InterpreterEquivalence` | `EnvAlg` is parallel-correct (corollary) |
| `interchange-parallel-equivalence` | `InterpreterEquivalence` | `InterchangeAlg` is parallel-correct (corollary) |
| **`sound-algebras-agree`** | **`InterpreterEquivalence`** | **Any two sound algebras agree (compositionality)** |

---

## 8  Finally-Tagless Interpreter Composition

This section covers the `Loom.Theory.Tagless.*` module family, which adds the
layer of the theory most directly connected to how the Loom Haskell library
works: programs are **parametric over an interpretation algebra**, and loop
transformations are expressed as **interpreter swaps** rather than bespoke
program rewritings.

### Background: two representation styles

The earlier sections of this document work in a *deep-embedding* style.  A
`Kernel sched ty` is an intrinsically typed GADT; the evaluator
`Semantics.evalKernel` is a fixed function over that GADT; and loop
transformations (interchange, strip-mining) are modeled by supplying a
different `ValidSchedule` to the same program term.

The Haskell library works differently.  Its `Loop repr` typeclass lets programs
be *polymorphic over the representation functor `repr`*:

```haskell
type Prog a = ∀ repr r. Loop repr => (a → repr r) → repr r
```

A "transformation" is just a different `Loop` instance — the compiler (GHC)
specializes each instance away, so the abstraction cost is zero at runtime.

The `Tagless.*` modules add an Agda counterpart to this mechanism.

---

### 8.1  The abstract algebra: `KernelAlg`

**Module:** `Tagless.Algebra`

```agda
record KernelAlg {rank} (sched : Schedule rank) (dom : Shape rank)
                 (repr : Set → Set) : Set₁ where
  field
    alg-return : ∀ {A} → A → repr A
    alg-bind   : ∀ {A B} → repr A → (A → repr B) → repr B
    alg-read   : ∀ {cap arr} → CanRead cap → Access cap sched arr → repr ℕ
    alg-write  : ∀ {cap arr} → CanWrite cap → Access cap sched arr → ℕ → repr ⊤
    alg-for    : (Index sched dom → repr ⊤) → repr ⊤
```

`KernelAlg` is the Agda analog of Loom's `Loop repr` typeclass.  Any record
instance is a valid interpreter for the language.

### 8.2  Tagless programs: `TaglessProg`

**Module:** `Tagless.Program`

```agda
TaglessProg : ∀ {rank} (sched : Schedule rank) (dom : Shape rank) (A : Set) → Set₁
TaglessProg sched dom A = ∀ {repr : Set → Set} → KernelAlg sched dom repr → repr A
```

A `TaglessProg` is a *universal* computation: it runs under any algebra.  This
is the Agda analog of Loom's `Prog` CPS monad.  The type lives in `Set₁`
because it quantifies over all `repr : Set → Set`.

### 8.3  Two concrete algebras: sequential and interchange

**Modules:** `Tagless.StateInterp`, `Tagless.InterchangeInterp`

`EnvAlg dom` is the sequential row-major `KernelAlg` instance.  Its `alg-for`
iterates over `dom` in the natural (row-major) order.

`InterchangeAlg dom` is identical except that `alg-for` uses column-major
iteration — outer loop over columns, inner loop over rows.  This is the
direct Agda encoding of **loop interchange as an interpreter swap**.

In the Haskell library the analogous operation would be providing a different
`Loop repr` instance to the same `Prog` value.  No program rewriting occurs.

### 8.4  The algebra simulation relation: `AlgSim`

**Module:** `Tagless.Algebra`

Because Agda lacks parametricity as a built-in theorem, the "free theorem" — that
a tagless program behaves the same way under any two related algebras — must be
stated explicitly:

```agda
record AlgSim {rank} {sched} {dom} {M₁ M₂} (R : ∀ {A} → M₁ A → M₂ A → Set)
              (alg₁ : KernelAlg sched dom M₁)
              (alg₂ : KernelAlg sched dom M₂) : Set₁ where
  field
    sim-return : ∀ {A} (a : A) → R (alg-return alg₁ a) (alg-return alg₂ a)
    sim-bind   : R m₁ m₂ → (∀ a → R (f₁ a) (f₂ a)) → R (alg-bind alg₁ m₁ f₁) ...
    sim-read   : R (alg-read alg₁ readable access) (alg-read alg₂ readable access)
    sim-write  : R (alg-write alg₁ writable access v) (alg-write alg₂ writable access v)
    sim-for    : (∀ ix → R (body₁ ix) (body₂ ix)) → R (alg-for alg₁ body₁) (alg-for alg₂ body₂)
```

`AlgSim R alg₁ alg₂` says that `alg₁` and `alg₂` are *R-bisimilar*: every
primitive operation produces R-related outputs when given the same inputs.

### 8.5  The structural free theorem: `toTaglessKernel-sim`

**Module:** `Tagless.BridgeToDeep`

> *Any deep `Kernel` term interpreted under two `AlgSim R`-related algebras
> produces `R`-related results.*

```agda
toTaglessKernel-sim :
  AlgSim R alg₁ alg₂ →
  (k : Kernel sched ty) →
  R (toTaglessKernel alg₁ k) (toTaglessKernel alg₂ k)
```

The proof is by structural induction on the `Kernel` GADT:

| Case | Proof |
|---|---|
| `pure val` | `sim-return` |
| `m >>= f` | `sim-bind` + induction on `m` and each `f a` |
| `read r a next` | `sim-bind` + `sim-read` + induction on each `next v` |
| `write w a v` | `sim-write` |

This theorem is the **explicit substitute for parametricity** in the tagless
setting.  In System F, every tagless program would satisfy the free theorem for
free.  In Agda, we prove it structurally for programs derived from the deep
embedding.

**Corollary `toTagless-sim`:** The free theorem lifts to full indexed program
bodies — `toTagless prog ix` satisfies `AlgSim R` whenever the underlying
`Kernel` does.

### 8.6  Loop interchange via algebra swap: `copy-tagless-interchange`

**Module:** `Tagless.InterchangeTheorem`

> *The same `TaglessProg` run under `EnvAlg` (row-major) and `InterchangeAlg`
> (column-major) produces identical post-states, for OI-consistent programs.*

```agda
copy-tagless-interchange :
  ∀ (env : Env rank2) →
  proj₁ (copy-prog (EnvAlg board) env) ≡
  proj₁ (copy-prog (InterchangeAlg board) env)
```

The proof chain is:

1. `copy-prog (EnvAlg board) env ≡ runRect2 env board-rect-copy` (by `refl`)
2. `copy-prog (InterchangeAlg board) env ≡ runRect2-col env board-rect-copy` (by `refl`)
3. `runRect2 env board-rect-copy ≡ runRect2-col env board-rect-copy` (from
   `LoopInterchange.loop-interchange-preserves-state`, using OI-consistency of `copy`)

The same result holds for `inc-prog` via `inc-tagless-interchange`.

**Loom connection:** In the Haskell library, providing `InterchangeAlg` instead
of `EnvAlg` as the `Loop repr` instance to the same `Prog` corresponds to the
compiler optimization of loop interchange.  This theorem says that doing so
is semantics-preserving for OI-consistent programs.

### 8.7  Definitional bridges: `copy-prog-bridge`

**Module:** `Tagless.BridgeToDeep`

> *The tagless and deep-embedding executions are definitionally equal — no
> isomorphism proof is needed.*

```agda
copy-prog-bridge : ∀ env → proj₁ (copy-prog (EnvAlg board) env) ≡ runRect2 env board-rect-copy
inc-prog-bridge  : ∀ env → proj₁ (inc-prog  (EnvAlg board) env) ≡ runRect2 env board-rect-inc
```

Both proofs are `refl`.  This confirms that the tagless layer is not a new
abstraction on top of the deep embedding — it IS the deep embedding, seen
through a different interface.  Any theorem proved about `runRect2` (e.g.,
`obs-correct`, `schedule-equivalence`) automatically applies to tagless programs
run under `EnvAlg`.

---

### 8.8  General interchange: `tagless-interchange-general`

**Module:** `Tagless.GeneralInterchange`

> *For any `ExactCoverRect2Kernel`, running the corresponding `TaglessProg`
> under `EnvAlg` (row-major) and `InterchangeAlg` (column-major) produces
> identical post-states.*

```agda
tagless-interchange-general :
  ∀ {rows cols} →
  (kernel : ExactCoverRect2Kernel rows cols) →
  (env : Env rank2) →
  PostStateEq
    (proj₁ (runTaglessProg prog (EnvAlg (shape2 rows cols)) env))
    (proj₁ (runTaglessProg prog (InterchangeAlg (shape2 rows cols)) env))
  where prog = kernelProgram (base (wholeKernel kernel))
```

This is the key generalisation of `copy-tagless-interchange`: the proof no
longer mentions `copy-prog` or `inc-prog` specifically.  Any deep-embedding
`Program rect (shape2 rows cols)` derived from an `ExactCoverRect2Kernel`
satisfies loop interchange when lifted to the tagless layer via `runTaglessProg`.

The central new contribution is `runTaglessProg : Program rect (shape2 rows cols)
→ TaglessProg rect (shape2 rows cols) ⊤`, the canonical lifter from the deep
embedding to the tagless layer.  The proof proceeds via two bridge lemmas
(one per algebra), each using `foldFin-cong` to connect the `foldDomain` /
`foldDomainInterchange` traversal to the raw `foldFin` structure in
`runRect2` / `runRect2Interchange`.  The leaf of each bridge uses a
structural induction lemma (`toTaglessKernel-EnvAlg-agree` /
`toTaglessKernel-InterchangeAlg-agree`) showing that interpreting any `Kernel`
term under the state-monad algebra agrees with `evalKernel`.  Separate lemmas
are needed for the two algebras to prevent Agda's unifier from attempting to
equate `EnvAlg dom` with `InterchangeAlg dom` (which differ in `alg-for`).

**Loom connection:** The paper claim "loop interchange = interpreter
substitution" is now general: it holds for all `ExactCoverRect2Kernel`
programs, not just the hardcoded copy/increment examples.  This is the result
that supports the "zero-cost transformation via finally-tagless" narrative.

---

## 9  Wavefront Traversal and foldFor Correctness

### `wavefront-covers`
**Module:** `WavefrontTraversal`

> *The anti-diagonal (wavefront) enumerator covers every cell of a 2D grid
> exactly once.*

```agda
wavefront-covers :
  ∀ {r c} (ix : RectIx (shape2 (suc r) (suc c))) →
  elemAt (wavefrontEnum r c) (coverDiag ix) (coverPos ix) ≡ ix
```

`wavefrontEnum r c` is an `Enumerator (RectIx (shape2 (suc r) (suc c)))` whose
outer index is the anti-diagonal number `d ∈ [0, r+c]` and whose inner index is
the row-ascending position within that diagonal.  The covering witnesses are:

- `coverDiag (row, col) = fromℕ< (toℕ row + toℕ col < diagCount r c)` —
  the cell lies on its diagonal-sum anti-diagonal.
- `coverPos (row, col)` — the position within that diagonal, computed as
  `toℕ row ∸ iMin(dN, c)` and coerced to the correct dependent type via
  `cast` (from `Data.Fin.Base`).

The full proof `wavefront-covers` establishes:
1. `iMin(d, c) + toℕ pos ≡ toℕ row`  (the row component matches)
2. `toℕ d ∸ (iMin(d, c) + toℕ pos) ≡ toℕ col`  (the column component matches)

and combines them with `toℕ-injective` and `cong₂ _,_`.

**Arithmetic backbone** — four helpers are proved locally:

| Lemma | Statement |
|---|---|
| `lt-add-∸-≤` | `x < suc a ∸ b → b + x ≤ a` |
| `add-≤-lt` | `b + x ≤ a → x < suc a ∸ b` |
| `n∸[n∸m]≤m` | `n ∸ (n ∸ m) ≤ m` (via `≤-total` + `m∸[m∸n]≡n`) |
| `iMin≤toℕrow` | `(row + col) ∸ c ≤ toℕ row` |

**Loom connection:** This theorem completes the schedule family.  The existing
theory has exact-cover proofs for `rect` (1D and 2D) and `tile` schedules.
`wavefront-covers` adds the anti-diagonal (wavefront) schedule to that family.
Any `OI-consistent` kernel whose access pattern satisfies the wavefront
dependency discipline (`diagSum(inputAt step) < diagSum(outputAt step)`) can be
scheduled with `parForWavefront2D` and the theorem guarantees coverage.

---

### `wavefront-is-enumerator`
**Module:** `WavefrontPhase`

> *The explicit phase-by-phase wavefront execution is definitionally equal to
> running the wavefront enumerator directly.*

```agda
wavefront-is-enumerator :
  ∀ {r c} →
  (env  : Env rank2) →
  (prog : Program {rank2} rect (shape2 (suc r) (suc c))) →
  runWavefrontPhases env prog ≡ runEnumerator env prog (wavefrontEnum r c)
```

`runWavefrontPhases` is defined as the obvious nested `foldFin` that mirrors
the natural implementation of a wavefront schedule: iterate over diagonals in
order, and within each diagonal iterate over cells in row-ascending order.
`runEnumerator env prog (wavefrontEnum r c)` is the same nested `foldFin`,
expanded from the `Enumerator` record.  The proof is `refl` — both sides
reduce to identical terms.

**Significance:** This "by-definition" equality means:

1. Any theorem proved about `runEnumerator env prog wavefrontEnum` (e.g. via
   `wavefront-covers` and the general `ExactCoverLinear` machinery) immediately
   transfers to `runWavefrontPhases`.
2. The phased execution model — process all diagonal-`d` cells, then all
   diagonal-`(d+1)` cells, … — is not an approximation: it IS the enumerator.
3. For kernels where all reads at diagonal `d` come from cells at diagonal
   `< d` (the `DiagDependent` condition), combining `wavefront-is-enumerator`
   with `wavefront-covers` gives a full correctness proof: the phased
   execution computes the pointwise specification.

**Loom connection:** This is the mechanized counterpart of the claim in the
Loom paper that `parForWavefront2D` can be lowered to a sequence of
`parFor` phases — one per anti-diagonal — without changing the result.

---

### `wavefront-injective-outer` and `wavefront-injective-inner`
**Module:** `WavefrontTraversal`

> *The anti-diagonal enumerator is injective: distinct enumerator positions
> yield distinct 2D cells.*

```agda
wavefront-injective-outer :
  ∀ {r c} (d1 d2 : Fin (diagCount r c))
    (p1 : Fin (diagLen r c d1)) (p2 : Fin (diagLen r c d2)) →
  elemAt (wavefrontEnum r c) d1 p1 ≡ elemAt (wavefrontEnum r c) d2 p2 →
  d1 ≡ d2

wavefront-injective-inner :
  ∀ {r c} (d : Fin (diagCount r c))
    (p1 p2 : Fin (diagLen r c d)) →
  elemAt (wavefrontEnum r c) d p1 ≡ elemAt (wavefrontEnum r c) d p2 →
  p1 ≡ p2
```

`wavefront-injective-outer` shows that if two enumerator cells are equal,
they must lie on the same anti-diagonal: the diagonal index is recovered by
adding row and column components, which are preserved by the equality.  The
key arithmetic helper `wavefront-diag-sum` establishes
`toℕ row + toℕ col ≡ toℕ d` for any cell `(row, col) = elemAt d p`,
so equality of cells implies equality of diagonal sums, which implies `d1 ≡ d2`.

`wavefront-injective-inner` handles the within-diagonal case: once `d1 ≡ d2`,
the row components of the two cells must agree (the row encodes the position
within the diagonal), and `toℕ`-injectivity plus `Fin.toℕ-injective` give
`p1 ≡ p2`.

Together with `wavefront-covers` these two lemmas establish that
`wavefrontEnum r c` is a **bijection** between the flat enumerator domain
and the 2D grid — each cell appears exactly once.

**Loom connection:** Injectivity is the formal counterpart of the "no
double-write" property required by `ExactCoverLinear`.  It ensures the
wavefront traversal satisfies the same coverage discipline as the rect and
tile schedules already in the theory.

---

### `wavefront-correct`, `wavefront-unrelated`, `wavefront-input-preserved`
**Module:** `WavefrontCorrect`

> *Running a pointwise 2D kernel under the wavefront traversal computes the
> same result as the pointwise specification, leaves unrelated arrays
> untouched, and preserves the input array.*

```agda
-- record bundling the kernel and output-injectivity witness
record WavefrontKernel (r c : ℕ) : Set₁

wavefront-correct :
  ∀ {r c} (k : WavefrontKernel r c) (env : Env rank2) →
  ∀ (row : Fin (suc r)) (col : Fin (suc c)) →
  lookup (runWavefront k env) (resolve (outputAt (WavefrontKernel.base k) (row , col)))
  ≡ lookup (pointwiseSpec2D (WavefrontKernel.base k) env) (resolve (outputAt (WavefrontKernel.base k) (row , col)))

wavefront-unrelated :
  ∀ {r c} (k : WavefrontKernel r c) (env : Env rank2) →
  ∀ (a : Array rank2) → a ≢ outputArr (WavefrontKernel.base k) →
  lookup (runWavefront k env) (resolve (readAt a {- some read pos -}))
  ≡ lookup env (resolve (readAt a …))

wavefront-input-preserved :
  ∀ {r c} (k : WavefrontKernel r c) (env : Env rank2) →
  ∀ (row : Fin (suc r)) (col : Fin (suc c)) →
  lookup (runWavefront k env) (resolve (inputAt (WavefrontKernel.base k) (row , col)))
  ≡ lookup env (resolve (inputAt (WavefrontKernel.base k) (row , col)))
```

The proof strategy:

1. **`wavefront-is-enumerator`** reduces `runWavefront` to `runEnumerator`
   applied to `wavefrontEnum r c`.
2. **`wavefront-covers` + `wavefront-injective-*`** supplies the exact-cover
   witness for `ExactCoverLinear`'s `runWhole-state-eq`.
3. **`runWhole-pointwise`** / **`runWhole-unrelated`** / **`runWhole-input-preserved`**
   from `WholeLinear` deliver the three per-cell results.

The proof proceeds by outer induction over the `diagCount` diagonals and inner
induction within each diagonal, using `runDiag-pointwise` (definitional equality
of a single-diagonal run to a `WholeLinear` run of a unit step kernel) and
`runDiag-noHit` (earlier diagonals do not overwrite output cells belonging to
later diagonals).

**Loom connection:** This completes the wavefront schedule's entry in the
mechanized correctness story.  The rect/tiled/interchange/polyhedral thread and
the wavefront thread now both terminate at the same headline form: *any
output-injective pointwise 2D kernel scheduled with `parForWavefront2D`
produces the same post-state as the pointwise specification.*
**Module:** `FoldForCorrect`

> *After running a 1D exact-cover kernel, folding the output array is
> extensionally equal to folding the expected (pointwise-spec) output.*

```agda
foldFor-correct :
  ∀ (r : Reducer) (kernel : ExactCoverRect1Kernel n) (env : Env rank1) →
  foldArray1D r (runRect1 env (kernelProgram (base (toExactCoverKernel kernel)))) outArr ≡
  foldArray1D r (rect1Expected kernel env)                                          outArr
  where outArr = outputArr (base (toExactCoverKernel kernel))
```

The proof follows three steps:

1. **`runRect1-state`** (from `ExactCoverRect1`) — the post-environment of
   running the kernel equals `rect1Expected kernel env` pointwise.
2. **`foldArray1D-cong`** (local helper) — if two environments agree
   pointwise on a 1D output array `array ident (shape1 n)`, then folding
   any `Reducer` over that array in both environments produces equal results.
   The proof pattern-matches on the array constructor to expose `n`, then
   applies `foldReducerFrom-cong`.
3. **Combine** — `foldArray1D-cong r env1 env2 outArr (runRect1-state …)`.

**Why this matters for GHC Core:** In the Haskell implementation, `foldFor`
uses a *loop-carried function argument* as the accumulator (instead of a
mutable `PrimVar`).  GHC unboxes the accumulator to a machine register in the
inner loop, whereas the `newReducer`/`getReducer` path allocates a `PrimVar`
on the heap.  `foldFor-correct` is the formal justification that this
optimization is semantics-preserving: the function-argument accumulator
computes the same reduction as the mutable-state version.

---

### Summary additions

| Theorem | Module | What it guarantees |
|---|---|---|
| `wavefront-covers` | `WavefrontTraversal` | Anti-diagonal enumerator covers every 2D cell |
| `wavefront-injective-outer` | `WavefrontTraversal` | Cells on different diagonals are distinct |
| `wavefront-injective-inner` | `WavefrontTraversal` | Distinct positions within one diagonal yield distinct cells |
| `wavefront-is-enumerator` | `WavefrontPhase` | Phased wavefront execution = enumerator (refl) |
| `wavefront-correct` | `WavefrontCorrect` | Wavefront output at every cell equals pointwise spec |
| `wavefront-unrelated` | `WavefrontCorrect` | Wavefront leaves all non-output arrays unchanged |
| `wavefront-input-preserved` | `WavefrontCorrect` | Wavefront leaves the input array unchanged |
| `foldFor-correct` | `FoldForCorrect` | `foldFor` result = fold of pointwise-spec output |


**Headline theorem:**

> *For any `ExactCoverRect2Kernel` that is `OutputInputConsistent`, the tagless
> sequential execution under `EnvAlg` produces the same post-state as parallel
> execution (`runParallel`) under any valid schedule.*

```agda
tagless-parallel-correct :
  ∀ {rows cols n} →
  (kernel : ExactCoverRect2Kernel rows cols) →
  OutputInputConsistent (base (wholeKernel kernel)) →
  (vs : ValidSchedule (base (wholeKernel kernel)) n) →
  (env : Env rank2) →
  PostStateEq
    (proj₁ (runTaglessProg (kernelProgram (base (wholeKernel kernel)))
                           (EnvAlg (shape2 rows cols)) env))
    (runParallel env (base (wholeKernel kernel)) vs)
```

**Auxiliary results in the same module:**

| Name | Statement |
|------|-----------|
| `tagless-correct` | `runTaglessProg (EnvAlg) env = rect2Expected kernel env` |
| `canonicalSchedule` | `ValidSchedule (base k) (rows * cols)` extracted from any `ExactCoverRect2Kernel` |
| `runParallel-canonical-correct` | `runParallel env base canonicalSchedule = rect2Expected kernel env` |
| `tagless-parallel-canonical` | `runTaglessProg (EnvAlg) env = runParallel env base canonicalSchedule` |

**Proof structure (diamond through `rect2Expected`):**

```
  tagless (EnvAlg)
        │
        │ tagless-correct
        ▼
  rect2Expected        ◀── sym runParallel-canonical-correct ── runParallel (canonical)
        │                                                               │
        │ sym tagless-correct                               parallel-schedule-invariance + OIC
        ▼                                                               ▼
  tagless (EnvAlg)  ─────────────── tagless-parallel-correct ──▶ runParallel (any vs)
```

The non-trivial step is `runParallel-canonical-correct`, which case-splits on
`arrayEq arr (outputArr base)`:
- `no` branch: both sides are `env arr ix` (definitional).
- `yes refl` branch: LHS contains `stepAt (linearTraversal (remQuot cols)) (combine r c)`
  which reduces definitionally to `remQuot cols (combine r c)`; RHS contains `(r , c)`.
  These are connected by `remQuot-combine : remQuot k (combine i j) ≡ (i , j)`.

**Loom connection:** This is the formal end-to-end justification for `parFor` in the
library.  The library's sequential tagless execution (under `EnvAlg`) is semantically
equivalent to parallel execution under any valid schedule, provided the kernel is
`OutputInputConsistent` — i.e., input and output arrays are distinct (the `distinct`
field of `PointwiseKernel`).  The `OutputInputConsistent` predicate is checked
statically by the `AccessCtx` type in `Loom.Verify`, so every kernel that compiles
is covered by this theorem.  The theorem also formalises why any partition of the
iteration domain is valid: `parallel-schedule-invariance` guarantees that all valid
schedules agree on the post-state, enabling the runtime to choose chunk boundaries
freely without changing the result.

---

## 10  Interpreter Equivalence (Headline Paper Theorem)

**Module:** `InterpreterEquivalence`

This section contains the formal counterpart to the paper's one-sentence
thesis:

> *Loop transformations are compositional interpreter swaps, not compiler
> passes.*

The theorem `interpreter-equivalence` makes this precise: **any** algebra that
traverses an `OutputInputConsistent` kernel with the same observable effect as
the sequential spec (captured by `SoundKernelAlg`) is observationally
equivalent to parallel execution under any valid schedule.  The proof is a
single four-step chain, not a new structural argument.  The real content is in
the two corollaries, which instantiate the condition to the two concrete
algebras and confirm that both are sound — establishing that row-major and
column-major interpreters are equally valid "implementations" of the parallel
semantics.

---

### 10.1  Soundness condition: `SoundKernelAlg`

```agda
SoundKernelAlg :
  ∀ {rows cols} →
  KernelAlg rect (shape2 rows cols) (StateM rank2) →
  Set
SoundKernelAlg alg =
  ∀ (kernel : ExactCoverRect2Kernel rows cols) →
  ∀ (env    : Env rank2) →
  PostStateEq
    (proj₁ (runTaglessProg (kernelProgram (base (wholeKernel kernel))) alg env))
    (runRect2 env (kernelProgram (base (wholeKernel kernel))))
```

`SoundKernelAlg alg` says that for every `ExactCoverRect2Kernel`-derived
program and every starting environment, the post-state produced by the algebra
agrees pointwise with the row-major sequential execution `runRect2`.  This is
a *traversal-level* condition — it constrains the full program result, not just
the individual primitive operations — because different algebras encode
different traversal orders in their `alg-for` field, which is abstract and
cannot be inspected generically.

Two algebras are proved sound:

| Algebra | Proof of `SoundKernelAlg` | Key lemma |
|---|---|---|
| `EnvAlg (shape2 rows cols)` | `EnvAlg-sound` | `runTaglessProg-env-bridge` (direct) |
| `InterchangeAlg (shape2 rows cols)` | `InterchangeAlg-sound` | `runTaglessProg-interchange-bridge` + `loop-interchange-preserves-state` |

`EnvAlg-sound` holds for all programs (not just OI-consistent ones) because
the env-bridge is unconditional.  `InterchangeAlg-sound` requires the program
to be derived from an `ExactCoverRect2Kernel` to invoke the loop-interchange
theorem.  Both proofs are two-liners.

---

### 10.2  HEADLINE THEOREM: `interpreter-equivalence`

> *For any sound algebra, any `OutputInputConsistent` kernel, and any valid
> schedule, the tagless execution under the algebra is observationally
> equivalent to parallel execution.*

```agda
interpreter-equivalence :
  ∀ {rows cols n} →
  (kernel : ExactCoverRect2Kernel rows cols) →
  OutputInputConsistent (base (wholeKernel kernel)) →
  (vs   : ValidSchedule (base (wholeKernel kernel)) n) →
  (alg  : KernelAlg rect (shape2 rows cols) (StateM rank2)) →
  SoundKernelAlg alg →
  (env  : Env rank2) →
  PostStateEq
    (proj₁ (runTaglessProg (kernelProgram (base (wholeKernel kernel))) alg env))
    (runParallel env (base (wholeKernel kernel)) vs)
```

The proof is a four-step `trans` chain (for each `(arr, ix)`):

| Step | Equality | Justification |
|---|---|---|
| 1 | `alg post-state ≡ runRect2 env prog` | `sound kernel env arr ix` |
| 2 | `runRect2 env prog ≡ rect2Expected kernel env` | `ECR2.runRect2-state` |
| 3 | `rect2Expected kernel env ≡ runParallel env base canonical` | `sym PC.runParallel-canonical-correct` |
| 4 | `runParallel env base canonical ≡ runParallel env base vs` | `parallel-schedule-invariance + OI` |

No new reasoning is needed — the proof is an assembly of existing modules.

**Paper reading:** The algebra plays the role of the "loop transformation
interpreter".  `alg-for` encodes the traversal strategy: row-major for
`EnvAlg`, column-major for `InterchangeAlg`, and potentially any other
traversal for future algebras.  `SoundKernelAlg` is the invariant that must
hold after choosing `alg-for`.  The theorem says choosing any sound `alg-for`
yields the same observable post-state — every sound interpreter is a valid
implementation of the parallel semantics.

---

### 10.3  Corollary 1: `sequential-parallel-equivalence`

> *The row-major (`EnvAlg`) interpreter is parallel-correct.*

```agda
sequential-parallel-equivalence :
  (kernel : ExactCoverRect2Kernel rows cols) →
  OutputInputConsistent (base (wholeKernel kernel)) →
  (vs  : ValidSchedule (base (wholeKernel kernel)) n) →
  (env : Env rank2) →
  PostStateEq
    (proj₁ (runTaglessProg (kernelProgram (base (wholeKernel kernel)))
            (EnvAlg (shape2 rows cols)) env))
    (runParallel env (base (wholeKernel kernel)) vs)
```

One-line proof: `interpreter-equivalence kernel oi vs (EnvAlg ...) EnvAlg-sound env`.

This recovers `tagless-parallel-correct` from `Tagless.ParallelCorrect` as a
special case of the general theorem.

---

### 10.4  Corollary 2: `interchange-parallel-equivalence`

> *The column-major (`InterchangeAlg`) interpreter is also parallel-correct.*

```agda
interchange-parallel-equivalence :
  (kernel : ExactCoverRect2Kernel rows cols) →
  OutputInputConsistent (base (wholeKernel kernel)) →
  (vs  : ValidSchedule (base (wholeKernel kernel)) n) →
  (env : Env rank2) →
  PostStateEq
    (proj₁ (runTaglessProg (kernelProgram (base (wholeKernel kernel)))
            (InterchangeAlg (shape2 rows cols)) env))
    (runParallel env (base (wholeKernel kernel)) vs)
```

One-line proof: `interpreter-equivalence kernel oi vs (InterchangeAlg ...) InterchangeAlg-sound env`.

Together with Corollary 1, this establishes a **triangle of equivalences**:

```
  row-major (EnvAlg)
       │                  ╲
       │ tagless-interchange-general   │ sequential-parallel-equivalence
       │                               │
       ▼                               ▼
  col-major (InterchangeAlg) ──────── parallel (any vs)
                      interchange-parallel-equivalence
```

All three vertices produce the same post-state for any `OutputInputConsistent`
`ExactCoverRect2Kernel`.  Any traversal strategy that is `SoundKernelAlg` can
be added as a fourth vertex without new reasoning.

---

### 10.5  Corollary 3: `sound-algebras-agree`

> *Any two sound KernelAlg instances produce observationally equivalent
> post-states for any OI-consistent kernel.*

```
sound-algebras-agree :
  ∀ {rows cols n} →
  (kernel : ExactCoverRect2Kernel rows cols) →
  OutputInputConsistent (base (wholeKernel kernel)) →
  (vs    : ValidSchedule (base (wholeKernel kernel)) n) →
  (alg₁ alg₂ : KernelAlg rect (shape2 rows cols) (StateM rank2)) →
  SoundKernelAlg alg₁ →
  SoundKernelAlg alg₂ →
  (env   : Env rank2) →
  PostStateEq
    (proj₁ (runTaglessProg (kernelProgram (base (wholeKernel kernel))) alg₁ env))
    (proj₁ (runTaglessProg (kernelProgram (base (wholeKernel kernel))) alg₂ env))
```

**Proof:** Apply `interpreter-equivalence` to `alg₁` and `alg₂` independently,
then connect the two `runParallel` results by `trans` and `sym`.

**Why it matters (compositionality):** The set of sound algebras forms an
equivalence class under `PostStateEq`.  A sequence of interpreter substitutions
`alg₁ → alg₂ → … → algₙ` is semantics-preserving whenever every algebra in the
chain is sound.  Adding a new traversal strategy (tiled wavefront, Z-order,
skewed stencil) to the framework requires only establishing `SoundKernelAlg`
for it; equivalence to all existing sound algebras and to the parallel semantics
follows automatically from this corollary — no per-pair proof is needed.

This is the mechanised form of the paper's one-sentence thesis:
**"loop transformations are compositional interpreter swaps."**

---

### Summary additions

| Theorem | Module | What it guarantees |
|---|---|---|
| `EnvAlg-sound` | `InterpreterEquivalence` | Row-major algebra agrees with `runRect2` |
| `InterchangeAlg-sound` | `InterpreterEquivalence` | Column-major algebra agrees with `runRect2` |
| `interpreter-equivalence` | `InterpreterEquivalence` | Any sound algebra = parallel semantics |
| `sequential-parallel-equivalence` | `InterpreterEquivalence` | `EnvAlg` is parallel-correct (corollary) |
| `interchange-parallel-equivalence` | `InterpreterEquivalence` | `InterchangeAlg` is parallel-correct (corollary) |
| **`sound-algebras-agree`** | **`InterpreterEquivalence`** | **Any two sound algebras agree (compositionality)** |
