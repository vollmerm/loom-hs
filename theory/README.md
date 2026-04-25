# Theory workspace

This directory contains the mechanized theory workspace for `loom-hs`.

The initial setup is a small, self-contained Agda project:

- `loom-hs-theory.agda-lib` defines the local Agda library,
- `libraries` pins the local library file set used by `make check`,
- `src/Loom/Theory/Main.agda` is the initial entry module,
- `Makefile` provides a basic typechecking target.

## Current theory surface

The initial mechanization now has a first rectangular+tiled core:

- `Loom.Theory.Prelude`
  - shared Agda imports and basic proof-oriented utilities
- `Loom.Theory.Shape`
  - `Rank`, `Shape`, and formal `Array` handles
- `Loom.Theory.Schedule`
  - `rect` and `tile` schedules for the first milestone
- `Loom.Theory.Index`
  - rectangular indices plus a tiled index view with explicit global reconstruction
- `Loom.Theory.Access`
  - `Capability`, `CanRead`, `CanWrite`, and schedule-typed access witnesses
- `Loom.Theory.Syntax`
  - a compact intrinsically typed kernel/program syntax for read/write kernels
- `Loom.Theory.Examples`
  - initial rectangular and tiled copy-style examples
- `Loom.Theory.Semantics`
  - typed array environments plus sequential evaluation for the current kernel fragment
- `Loom.Theory.Safety`
  - initial shape/schedule safety lemmas and a first read-after-write property
- `Loom.Theory.Pointwise`
  - a proof-facing class of single-write pointwise kernels plus generic `runAt` lemmas
- `Loom.Theory.TiledPointwise`
  - a tiled single-step theorem layer phrased directly in terms of resolved global coordinates
- `Loom.Theory.WholeTiled`
  - a whole-program tiled correctness layer over explicit finite tiled iteration streams
- `Loom.Theory.ExactCoverTiled`
  - a tiled whole-program theorem layer strengthened with an exact-cover witness over outputs
- `Loom.Theory.ExactCoverRect1`
  - an exact-cover whole-program theorem layer for 1D rectangular kernels
- `Loom.Theory.ExactCoverRect2`
  - an exact-cover whole-program theorem layer for 2D rectangular kernels
- `Loom.Theory.LoopInterchange`
  - a column-major rectangular execution semantics plus a loop-interchange post-state equivalence theorem
- `Loom.Theory.StripMineTiling`
  - a first strip-mined exact-cover tiled traversal equivalence theorem against rectangular execution
- `Loom.Theory.SchedulePreservation`
  - a first schedule-transformation preservation layer for equivalent exact-cover tiled traversals
- `Loom.Theory.Determinism`
  - a packaged post-state equivalence theorem for disciplined equivalent tiled traversals
- `Loom.Theory.Reduction`
  - a minimal sequential reducer semantics plus 1D array-facing fold operations
- `Loom.Theory.ReductionTheorems`
  - first correctness results connecting reducer execution to an explicit recursive sum
- `Loom.Theory.Traversal`
  - first-class extracted-friendly traversal enumerators for rectangular and explicit tiled traversals
- `Loom.Theory.Phase`
  - a structured phased-program layer for sequencing whole rectangular phases
- `Loom.Theory.PhaseSemantics`
  - reference semantics for phased execution, treating phase boundaries as coarse barriers
- `Loom.Theory.PhaseTheorems`
  - congruence and composition lemmas showing phased execution preserves post-state equivalence
- `Loom.Theory.ProgramTheorems`
  - first program-level correctness theorems for rectangular and tiled copy bodies
- `Loom.Theory.RectExecution`
  - whole-program sequential execution for rectangular 1D and 2D kernels
- `Loom.Theory.FullRunTheorems`
  - pointwise full-run theorems for the tiny rectangular execution example
- `Loom.Theory.WholeLinear`
  - **generic** whole-program pointwise theorem, parameterized by rank, schedule, domain, and a
    `LinearTraversal {A = Index sched dom} n`; the unifying generalization over `WholeRect1` and
    `WholeTiled`
- `Loom.Theory.WholeRect1`
  - 1D rectangular whole-program pointwise theorem; now a thin wrapper over `WholeLinear`
- `Loom.Theory.WholeRect2`
  - the corresponding generic whole-program pointwise theorem for 2D rectangular kernels (nested
    row/column induction); exposes `toWholeKernel` and `runRect2-eq-runWhole` bridges to
    `WholeLinear` via the row-major `Fin rows × Fin cols ↔ Fin (rows * cols)` flattening bijection
- `Loom.Theory.ExactCoverLinear`
  - **generic** exact-cover state-characterization theorem, parameterized by a `WholeLinear.WholeKernel`;
    `runWhole-state-eq` is the unified whole-run correctness statement for 1D rectangular, tiled,
    and (via flattening) 2D rectangular instances; all three families now bridge into this layer
- `Loom.Theory.ObservationalEquivalence`
  - **paper-facing headline theorem**: `obs-correct` states that any `WellFormedKernel` (satisfying
    access discipline + coverage discipline) is observationally equivalent to its `pointwiseSpec`;
    a thin re-packaging of `ExactCoverLinear.runWhole-state-eq` with paper-friendly names, explicit
    discipline documentation, and a concrete 1D example
- `Loom.Theory.ScheduleIndependent`
  - defines `ValidSchedule`, the factoring of `ExactCoverKernel` into a base kernel and a
    traversal schedule; provides `OutputInputConsistent` (the "no carried cross-iteration dependence"
    condition); includes bridges `toWholeKernel` and `toExactCoverKernel`; proves
    `OutputInputConsistent` holds for all concrete example kernels (copy and increment, 1D and 2D
    rect and tiled); adds `factoring-implies-oi-consistent` (if the input-access function factors
    through the output-access function via some map `f`, then `OutputInputConsistent` holds
    automatically — the abstract form of the polyhedral "input index is a function of output index"
    check) and `line-copy-oi-by-factoring` as a concrete corollary via `f = id`
- `Loom.Theory.ScheduleEquivalence`
  - **schedule equivalence theorem**: for any `OutputInputConsistent` base kernel, any two valid
    schedules produce identical post-states; the three-step proof uses `schedule-canonical-output`
    (per-cell correctness from `ExactCoverLinear`), `schedule-independent-spec` (the key lemma that
    different schedules write the same value, by `OutputInputConsistent`), and transitivity
- `Loom.Theory.PolyhedralModel`
  - **polyhedral model connection**: paper-facing packaging of `schedule-equivalence` as
    `polyhedral-schedule-invariance`; defines `LegalSchedule` as the Loom analog of a polyhedral-
    legal schedule; includes a concrete forward-vs-backward ordering corollary for `line-copy-kernel`
- `Loom.Theory.KernelIndependence`
  - **kernel independence theorem**: `kernel-independence` proves that two `ExactCoverKernel`
    instances with disjoint output arrays that do not read from each other's outputs can be run in
    either order (`runWhole k1` then `runWhole k2`, or `runWhole k2` then `runWhole k1`) and produce
    the same combined environment; proof uses `runWhole-unrelated` and `runWhole-covered-pointwise`
    in a three-case analysis on which array is queried; a concrete demo `line-copy-inc-independent`
    in `Main.agda` shows that `line-exact-kernel` (writes array-6) and `line-inc-exact-kernel`
    (writes array-7) commute with all three non-overlap hypotheses discharging as `λ ()`

### 2D flattening via `foldFin-product`

All three `ExactCover*` families now share a single proof spine in `ExactCoverLinear`. Connecting
`WholeRect2` to `WholeLinear` required a bijection between a 2D iteration domain and a flat linear
index. The approach uses the stdlib `combine : Fin m → Fin n → Fin (m * n)` /
`remQuot : ∀ n → Fin (m * n) → Fin m × Fin n` pair from `Data.Fin.Base`.

The key new lemma in `Loom.Theory.Traversal` is:

```agda
foldFin-product :
  ∀ {A : Set} {rows cols : ℕ} →
  (f : Fin rows → Fin cols → A → A) →
  (init : A) →
  foldFin (λ i acc → foldFin (f i) acc) init ≡
  foldFin (λ flat acc → let (i , j) = remQuot cols flat in f i j acc) init
```

This says nested `foldFin` over `rows × cols` steps equals a single flat `foldFin` over
`rows * cols` steps, with 2D coordinates recovered via `remQuot`. The proof is by induction on
`rows`, using two auxiliary lemmas:

- `foldFin-append`: splits a flat fold over `outer + inner` steps into an inner-first,
  outer-second pass, using the `_↑ˡ_` / `_↑ʳ_` index injections.
- `foldFin-cong`: rewrites the body of a flat fold pointwise (the fold analogue of `cong`).

`remQuot-↑ˡ` and `remQuot-↑ʳ` connect `remQuot` applied to left/right-injected indices back to
`remQuot-combine` from `Data.Fin.Properties`.

The bridge `WholeRect2.toWholeKernel` uses `Traversal.linearTraversal (remQuot cols)` as the step
function so the flat index unfolds to the correct 2D coordinate. The bridge
`WholeRect2.runRect2-eq-runWhole` is then a direct one-line application of `foldFin-product`.
`ExactCoverRect2.toExactCoverKernel` maps `(coverRow, coverCol)` to `combine row col` and recovers
the cover witness via `remQuot-combine`.



The Agda modules deliberately mirror a smaller subset of the Haskell verification-facing API:

| Agda module | Main concepts | Related Haskell concepts |
| --- | --- | --- |
| `Shape` | `Rank`, `Shape`, `Array` | `Rank`, `Shape`, `Array` |
| `Schedule` | `rect`, `tile` | `Schedule` without wavefront yet |
| `Index` | `RectIx`, `TileIx`, `Index` | `Index`, `TileIndex2` |
| `Access` | capability-indexed accesses | `Capability`, `AccessCtx` |
| `Syntax` | typed kernel/program fragment | verified loop bodies and array actions |
| `Semantics` | access resolution, typed environments, sequential evaluation | reference model for future kernel semantics |
| `Safety` | first intrinsic lemmas | shape/schedule consistency guarantees |
| `Pointwise` | proof-facing pointwise kernel class | reusable single-step theorem layer |
| `TiledPointwise` | tiled/global single-step theorem layer | schedule-facing correctness over tiled accesses |
| `WholeTiled` | whole-program tiled correctness; exposes `toWholeKernel` bridge to `WholeLinear` | induction over explicit tiled iteration schedules |
| `ExactCoverTiled` | exact-cover tiled correctness; exposes `toExactCoverKernel` bridge to `ExactCoverLinear` | whole-run pointwise theorem for every covered output coordinate |
| `ExactCoverRect1` | exact-cover 1D rectangular correctness; exposes `toExactCoverKernel` bridge to `ExactCoverLinear` | whole-run state characterization for 1D rectangular kernels |
| `ExactCoverRect2` | exact-cover 2D rectangular correctness; exposes `toExactCoverKernel` bridge to `ExactCoverLinear` via `combine`/`remQuot` flattening | whole-run state characterization for 2D rectangular kernels |
| `WholeLinear` | **generic** whole-program pointwise theorem over any `LinearTraversal` | parameterized by rank, schedule, domain; unifies `WholeRect1` and `WholeTiled` |
| `ExactCoverLinear` | **generic** exact-cover state-characterization theorem over `WholeLinear.WholeKernel` | `runWhole-state-eq` is the unified whole-run correctness statement |
| `LoopInterchange` | alternate rectangular traversal semantics | loop-interchange equivalence for 2D rectangular kernels |
| `StripMineTiling` | rectangular vs tiled exact-cover equivalence | first strip-mining / tiling post-state equivalence theorem |
| `SchedulePreservation` | schedule-transformation preservation | semantic equality for equivalent exact-cover tiled traversals |
| `Determinism` | post-state equivalence for disciplined traversals | first paper-facing determinism/race-freedom stepping stone |
| `ScheduleIndependent` | `ValidSchedule`, `OutputInputConsistent`, bridges | factoring ExactCoverKernel into base + schedule; the "no carried dependence" condition |
| `ScheduleEquivalence` | **general schedule-equivalence theorem** | any two valid schedules for an `OutputInputConsistent` kernel produce the same post-state |
| `PolyhedralModel` | **polyhedral model connection** | `polyhedral-schedule-invariance`; `LegalSchedule`; forward-vs-backward concrete corollary |
| `KernelIndependence` | **kernel independence theorem** | `kernel-independence`: two kernels with disjoint outputs that don't cross-read commute under `runWhole` composition; the formal basis for parallel composition of independent `parFor` regions |
| `Reduction` | sequential reducer semantics | proof-friendly model for `foldFor` / `foldFor1D` style reductions |
| `ReductionTheorems` | first reducer correctness results | explicit summation semantics for `sumReducer` |
| `ConfluenceReduction` | comm+assoc ⟹ `foldReducer` is permutation-invariant | `foldReducer-perm` theorem; underpins `foldFor` correctness |
| `FoldForCorrect` | **foldFor correctness theorem** | `foldFor-correct`: after running a 1D exact-cover kernel, folding the output = folding the pointwise-spec output |
| `WavefrontTraversal` | **anti-diagonal enumerator + coverage + injectivity** | `wavefrontEnum r c`; `wavefront-covers` (surjectivity); `wavefront-injective-outer` / `wavefront-injective-inner` (injectivity) — together a bijection proof |
| `WavefrontPhase` | **phased wavefront = enumerator (definitional)** | `wavefront-is-enumerator`: `runWavefrontPhases ≡ runEnumerator wavefrontEnum` by `refl` |
| `WavefrontCorrect` | **wavefront pointwise correctness** | `wavefront-correct`, `wavefront-unrelated`, `wavefront-input-preserved`: the three-part whole-program correctness theorem for 2D pointwise kernels under the anti-diagonal schedule |
| `Traversal` | first-class traversal enumerators | extraction-friendly traversal descriptors for verified execution order |
| `Phase`, `PhaseSemantics`, `PhaseTheorems` | phased rectangular programs and barrier-style composition | structured phase sequencing corresponding to coarse barrier boundaries |
| `ProgramTheorems` | program-level correctness facts | whole-body reasoning over verified kernels |
| `RectExecution` | whole-program rectangular execution | sequential traversal of rectangular kernels |
| `WholeRect1` | generic whole-program theorem | first induction proof over complete execution |
| `WholeRect2` | generic 2D whole-program theorem; exposes `toWholeKernel` / `runRect2-eq-runWhole` bridges to `WholeLinear` via `foldFin-product` | nested induction proof over complete execution; flattened to generic layer |
| `FullRunTheorems` | pointwise whole-program facts | first full-run results over complete executions |

This is intentionally a correspondence layer rather than a full formal clone of `Loom.Verify`.
Richer reducer effects and runtime details remain deferred. Barriers are now modeled
only in a structured way, as separators between whole rectangular phases rather than as arbitrary
kernel effects. The wavefront correctness theorem is now fully mechanized.

### Current API coverage audit

The current Agda core most directly mirrors these parts of `Loom.Verify`:

| `Loom.Verify` surface | Agda status | Main Agda modules |
| --- | --- | --- |
| `Rank`, `Shape`, `Array`, `Index` | covered for 1D and 2D | `Shape`, `Index` |
| `Schedule`, rectangular traversal, tiled 2D traversal | covered for `rect` and 2D `tile` | `Schedule`, `RectExecution`, `WholeTiled` |
| `Capability`, access witnesses, read/write discipline | covered for read/write capabilities | `Access`, `Syntax`, `Semantics` |
| `readAt`, `writeAt`-style verified array actions | covered as typed kernel primitives | `Syntax`, `Semantics` |
| `parFor1D`, `parFor2D` | covered via first-class traversal enumerators, sequential whole-program reference execution, exact-cover rectangular state theorems, a first interchange theorem for 2D, and a generic `LinearTraversal`-indexed whole-program correctness layer | `Traversal`, `RectExecution`, `WholeLinear`, `WholeRect1`, `WholeRect2`, `ExactCoverLinear`, `ExactCoverRect1`, `ExactCoverRect2`, `LoopInterchange` |
| `parForTiled2D` | covered via tiled single-step, whole-program theorems, a generic `LinearTraversal` bridge, and a first strip-mined equivalence result against rectangular execution | `TiledPointwise`, `WholeTiled`, `ExactCoverTiled`, `WholeLinear`, `ExactCoverLinear`, `StripMineTiling`, `Determinism` |
| `foldFor1D`, reducer story | covered by sequential reducer semantics, permutation-invariance theorem, and `foldFor-correct` (fold of kernel output = fold of pointwise spec) | `Reduction`, `ReductionTheorems`, `ConfluenceReduction`, `FoldForCorrect` |
| `parallel` regions separated by `barrier` | partially covered for rectangular kernels via phased-program sequencing over the shared store model | `Phase`, `PhaseSemantics`, `PhaseTheorems` |
| `parForWavefront2D` | **mechanized**: anti-diagonal exact-cover (`wavefront-covers`), enumerator injectivity (`wavefront-injective-outer`, `wavefront-injective-inner`), definitional equality of phased/enumerator execution (`wavefront-is-enumerator`), and full per-cell pointwise correctness (`wavefront-correct`) | `WavefrontTraversal`, `WavefrontPhase`, `WavefrontCorrect` |

The current Agda theory does **not** yet model these `Loom.Verify` features directly:

| `Loom.Verify` surface | Current status |
| --- | --- |
| `shape3`, `rectIx3`, `parFor3D`, `parForTiled3D`, `unIndex3` | not yet modeled |
| `parForWavefront2D`, `WaveOffset`, `readWaveAt`, `writeWaveAt`, `waveCoordsOf` | **mechanized**: anti-diagonal enumerator surjectivity, injectivity, and full per-cell pointwise correctness of the wavefront schedule proved in `WavefrontTraversal`, `WavefrontPhase`, and `WavefrontCorrect`; DiagDependent kernel effects (direct anti-diagonal dependence between iterations) remain outside scope |
| `AccessCtx` as a richer explicit context API | simplified into capability-indexed access witnesses |
| arbitrary `barrier` placement, nested parallel runtime behavior | still intentionally outside the current sequential reference model |
| `DVec` and vectorized array helpers | intentionally omitted from the mechanized core |
| `newReducer`, `reduce`, `getReducer` as mutable reducer operations | currently represented only by a sequential mathematical reducer model |

For paper purposes, this is the intended split: the mechanization aims to justify the
verification-facing design principles and theorem story, not to reproduce every library
feature or low-level runtime operation one-for-one.

### Aligned example set

The current paper-facing examples are meant to stay synchronized across the mechanization and
the `Loom.Verify` story:

| Agda example family | Intended `Loom.Verify` narrative |
| --- | --- |
| `line-rect-copy`, `line-rect-inc` | simple `parFor1D` / `foldFor1D` kernels over a 1D verified shape |
| `tiny-rect-copy`, `tiny-rect-inc` | small `parFor2D` kernels with whole-run rectangular reasoning |
| `tiled-copy` | `parForTiled2D` with exact-cover traversal reasoning and schedule invariance |

This gives the paper a compact kernel set that can be presented once and then reused across the
API explanation, Agda theorems, and benchmark-facing discussion.

### Structured barrier extension

The theory now has a first barrier-facing extension, but it is intentionally coarse:

- a kernel body is still barrier-free,
- a phased program is a sequence of whole rectangular phases,
- and the boundary between phases is the semantics of a coarse global barrier.

This matches the intended proof story for `Loom.Verify`: the Agda development justifies a
structured staged-execution fragment without trying to formalize the full worker/runtime
synchronization behavior of the Haskell implementation.

### Traversal extraction direction

The theory now includes a first-class traversal enumerator layer. The immediate purpose of this
layer is to make execution order explicit in a form that is:

- reusable across rectangular and explicit tiled execution,
- directly connected to the current exact-cover and transformation theorems,
- and structurally suitable for later Agda-to-Haskell extraction.

Both the 2D rectangular execution layer and the explicit tiled whole-program / transformation
layers now route through this shared traversal story:

- rectangular execution uses canonical row-major and column-major enumerators,
- tiled execution uses a shared linear-traversal wrapper over enumerators,
- and the schedule-preservation / determinism theorems now store traversal objects rather than
  raw one-off step functions.

The next extraction-oriented step on the tiled side is not a representation refactor anymore; it
is to extend this shared traversal layer into the phased-program story so tiled phases can reuse
the same barrier-style composition machinery as rectangular phases.

The repository now also contains an extraction-oriented path for the rectangular traversal layer:

- `Loom.Theory.Traversal.Export` exposes concrete Agda coordinate lists for canonical traversals,
- Agda compilation produces generated Haskell under `theory/src/MAlonzo/Code/`,
- and `theory/extracted/Loom/Verify/Traversal.hs` is a thin handwritten wrapper around the
  generated module.

The current wrapper exposes:

- `rect1Coords`
- `rowMajorCoords2D`
- `columnMajorCoords2D`

This is intentionally a metadata path, not a runtime replacement.

To regenerate the extracted Agda Haskell for this path, run:

```bash
cd theory
make compile-traversal-export
ghc -fno-code -itheory/src -itheory/extracted theory/extracted/Loom/Verify/Traversal.hs
```

The first command regenerates the MAlonzo output for `Loom.Theory.Traversal.Export`. The second
checks the thin Haskell wrapper that consumes the generated module.

### N-dimensional outlook

The Haskell verification-facing API already exposes 3D loop forms such as `shape3`, `rectIx3`,
`parFor3D`, and `parForTiled3D`.

The current Agda mechanization does **not** yet prove arbitrary-rank results. It establishes the
theorem story concretely for 1D and 2D fragments, especially:

- exact-cover whole-run state characterization for rectangular kernels,
- exact-cover whole-run pointwise/state reasoning for tiled 2D traversals,
- post-state equivalence for disciplined equivalent tiled traversals, and
- a **unified generic layer** (`WholeLinear` / `ExactCoverLinear`) that all three families
  (1D rectangular, tiled, and 2D rectangular) now bridge into via a flat index bijection.

For the paper, the defensible general statement is that this proof architecture appears to scale to
finite product domains of higher rank, but only the 1D/2D fragments are mechanized today. A future
rank-polymorphic development would need generic shape/index/traversal induction rather than the
current hand-rolled 1D and 2D inductions.

## Paper-facing theorem ladder

The mechanization now has a clearer proof ladder for a paper-oriented story:

0. **headline theorem**: any well-formed kernel (access + coverage discipline) is observationally
   equivalent to its pointwise specification (`ObservationalEquivalence.obs-correct`)
1. single-step pointwise correctness (`Pointwise`, `TiledPointwise`)
2. whole-program exact-cover correctness (`WholeRect1`, `WholeRect2`, `ExactCoverRect1`, `ExactCoverRect2`, `WholeTiled`, `ExactCoverTiled`)
3. first rectangular schedule-transformation result via loop interchange (`LoopInterchange`)
4. first rectangular-to-tiled strip-mining equivalence result (`StripMineTiling`)
5. schedule-invariant output preservation for equivalent disciplined traversals (`SchedulePreservation`)
6. packaged post-state equivalence for those traversals (`Determinism`)
7. phased rectangular composition over coarse barrier boundaries (`Phase`, `PhaseSemantics`, `PhaseTheorems`)
8. **polyhedral model core theorem**: all legal schedules for any `OutputInputConsistent` kernel
   produce identical post-states (`ScheduleIndependent`, `ScheduleEquivalence`, `PolyhedralModel`)

Steps 3–6 are specific instances of step 8.  The `schedule-equivalence` theorem in
`ScheduleEquivalence` subsumes loop interchange, strip-mine tiling, and schedule preservation
as corollaries: all three are instances of two valid schedules for an `OutputInputConsistent`
base kernel.

The `ObservationalEquivalence` module re-packages the generic `ExactCoverLinear.runWhole-state-eq`
proof under paper-friendly names.  The key concepts it exposes are:

| Paper name | Agda name | Meaning |
|---|---|---|
| `WellFormedKernel` | `ExactCoverKernel` | bundled access + coverage discipline |
| `pointwiseSpec` | `expectedOutput` | the per-cell specification function |
| `run` | `runWhole ∘ wholeKernel` | whole-program execution |
| `obs-correct` | `runWhole-state-eq` | the headline observational equivalence |
| `obs-correct-at` | `runWhole-covered-pointwise` | per-output-cell corollary |
| `obs-correct-unrelated` | `runWhole-unrelated` | unrelated-array preservation |

The `PolyhedralModel` module re-packages `ScheduleEquivalence.schedule-equivalence` under
polyhedral-model-facing names:

| Paper name | Agda name | Meaning |
|---|---|---|
| `LegalSchedule` | `LegalSchedule` | base + `OutputInputConsistent` + `ValidSchedule` |
| `polyhedral-schedule-invariance` | `schedule-equivalence` | all legal schedules agree |
| `OutputInputConsistent` | `OutputInputConsistent` | no cross-iteration RAW/WAW dependence |
| `ValidSchedule` | `ValidSchedule` | exact-cover execution order |

The current main targets beyond this are to strengthen the affine/transformation story beyond loop
interchange, to extend the determinism layer into a more explicit race-freedom or disciplined
parallel safety theorem, and to continue extending the reduction story enough to support the
paper's `foldFor` claims.

## Typecheck the theory

From this directory, run:

```bash
make check
```

## Next steps

The next modules should extend this core with:

- canonical tiled traversals and stronger schedule-preserving transformation theorems,
- richer reducer correctness and eventually transformation theorems beyond exact-cover tiled traversals,
- additional computed-kernel examples beyond copy,
- extending the structured phased layer from rectangular phases to tiled phases,
- and later, optional wavefront support.
