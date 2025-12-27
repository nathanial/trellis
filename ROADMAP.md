# Trellis Roadmap

This document outlines feature proposals, code improvements, and cleanup tasks for the Trellis CSS layout library.

---

## Feature Proposals

### [COMPLETED] CSS Flexbox `flex-wrap` Multi-line Layout Improvements
**Status:** ✅ Implemented

**What was done:**
- Fixed `wrap-reverse` to correctly reverse cross-axis direction (not just line order)
- Modified `alignFlexLines` to position lines from cross-end for wrap-reverse mode
- Added 10 comprehensive tests for multi-line flex layouts

**Files Changed:**
- `Trellis/Algorithm.lean` - Fixed `partitionIntoLines` and `alignFlexLines`
- `TrellisTests/Main.lean` - Added flex-wrap test suite

---

### [COMPLETED] CSS Grid `minmax()` Track Sizing
**Status:** ✅ Implemented

**What was done:**
- Added `minSize` and `maxSize` fields to `ResolvedTrack` structure
- Updated `initTracks` to extract fr values from minmax and resolve min/max constraints
- Updated `sizeTracksToContent` to properly size minmax tracks (base at min, grow to content up to max)
- Updated `resolveFrTracks` to handle minmax(min, fr) tracks in fr distribution
- Added 6 comprehensive tests for minmax() behavior

**Files Changed:**
- `Trellis/Algorithm.lean` - Core minmax logic in track sizing
- `TrellisTests/Main.lean` - Added minmax test suite

---

### [COMPLETED] CSS Grid `repeat()` Function Support
**Status:** ✅ Implemented

**What was done:**
- Added `RepeatMode` enum for `count`, `autoFill`, and `autoFit` modes
- Added `TrackEntry` type to support both single tracks and repeat entries
- Modified `GridTemplate` to support `entries` array alongside legacy `tracks`
- Implemented `expandEntries` to flatten repeat patterns into track arrays
- Implemented `calculateAutoRepeatCount` for auto-fill/auto-fit responsive grids
- Updated `layoutGridContainer` to use expanded track counts for grid sizing
- Added 7 comprehensive tests for repeat() behavior

**Files Changed:**
- `Trellis/Grid.lean` - Added `RepeatMode`, `TrackEntry`, modified `GridTemplate`
- `Trellis/Algorithm.lean` - Added expansion and auto-repeat logic
- `TrellisTests/Main.lean` - Added repeat() test suite

---

### [Priority: Medium] CSS Grid Named Lines and Areas
**Description:** Implement support for named grid lines and `grid-template-areas` for semantic grid definitions. Currently, `GridLine.named` exists but is not processed (returns 0 in `resolveGridLine`).

**Rationale:** Named lines and areas make complex grid layouts more maintainable and readable.

**Affected Files:**
- `/Users/Shared/Projects/lean-workspace/trellis/Trellis/Grid.lean` (lines 68-73, `GridLine`)
- `/Users/Shared/Projects/lean-workspace/trellis/Trellis/Algorithm.lean` (lines 457-470, 486-505)

**Estimated Effort:** Large

**Dependencies:** None

---

### [Priority: Medium] Flexbox `order` Property
**Description:** Add support for the CSS `order` property that allows reordering flex items visually without changing the DOM order.

**Rationale:** The `order` property is commonly used for responsive designs where visual order differs from source order.

**Affected Files:**
- `/Users/Shared/Projects/lean-workspace/trellis/Trellis/Flex.lean` (add `order` field to `FlexItem`)
- `/Users/Shared/Projects/lean-workspace/trellis/Trellis/Algorithm.lean` (sort items by order before layout)

**Estimated Effort:** Small

**Dependencies:** None

---

### [Priority: Medium] CSS `aspect-ratio` Property
**Description:** Add support for the CSS `aspect-ratio` property to maintain width-to-height ratios during layout calculation.

**Rationale:** Aspect ratio constraints are essential for responsive images, videos, and maintaining consistent proportions.

**Affected Files:**
- `/Users/Shared/Projects/lean-workspace/trellis/Trellis/Types.lean` (add `aspectRatio` to `BoxConstraints`)
- `/Users/Shared/Projects/lean-workspace/trellis/Trellis/Algorithm.lean` (apply aspect ratio during dimension resolution)

**Estimated Effort:** Medium

**Dependencies:** None

---

### [Priority: Medium] Margin Collapse Support
**Description:** Implement CSS margin collapsing behavior where adjacent vertical margins combine into a single margin equal to the larger of the two.

**Rationale:** Margin collapse is a fundamental CSS behavior that affects vertical spacing between elements.

**Affected Files:**
- `/Users/Shared/Projects/lean-workspace/trellis/Trellis/Algorithm.lean` (new margin collapse logic)
- `/Users/Shared/Projects/lean-workspace/trellis/Trellis/Types.lean` (possibly add `marginCollapse` option)

**Estimated Effort:** Medium

**Dependencies:** None

---

### [Priority: Medium] CSS Grid `subgrid` Support
**Description:** Implement `subgrid` to allow nested grids to participate in their parent's track sizing.

**Rationale:** Subgrid enables consistent alignment across nested grid structures.

**Affected Files:**
- `/Users/Shared/Projects/lean-workspace/trellis/Trellis/Grid.lean` (new `Subgrid` track type)
- `/Users/Shared/Projects/lean-workspace/trellis/Trellis/Algorithm.lean` (subgrid resolution logic)

**Estimated Effort:** Large

**Dependencies:** None

---

### [Priority: Low] Flexbox `visibility: collapse` Support
**Description:** Support the `visibility: collapse` behavior for flex items where the item is hidden but its cross-size contribution to the line is preserved.

**Rationale:** Useful for implementing togglable content without layout shifts.

**Affected Files:**
- `/Users/Shared/Projects/lean-workspace/trellis/Trellis/Flex.lean` (add visibility property)
- `/Users/Shared/Projects/lean-workspace/trellis/Trellis/Algorithm.lean`

**Estimated Effort:** Small

**Dependencies:** None

---

### [Priority: Low] Debug/Introspection API
**Description:** Add an API to inspect intermediate layout state (flex lines, track sizes, item measurements) for debugging purposes.

**Rationale:** Debugging layout issues is challenging without visibility into intermediate calculations.

**Affected Files:**
- `/Users/Shared/Projects/lean-workspace/trellis/Trellis/Algorithm.lean` (new debug result types)
- New file: `/Users/Shared/Projects/lean-workspace/trellis/Trellis/Debug.lean`

**Estimated Effort:** Medium

**Dependencies:** None

---

### [Priority: Low] Layout Caching/Memoization
**Description:** Implement layout caching to avoid recomputing layouts for unchanged subtrees.

**Rationale:** Performance optimization for large layout trees with incremental updates.

**Affected Files:**
- `/Users/Shared/Projects/lean-workspace/trellis/Trellis/Node.lean` (add cache key/hash)
- `/Users/Shared/Projects/lean-workspace/trellis/Trellis/Algorithm.lean` (caching logic)

**Estimated Effort:** Large

**Dependencies:** None

---

## Code Improvements

### [COMPLETED] Replace Magic Number for Unbounded Max
**Status:** ✅ Implemented

**What was done:**
- Added `Length.unbounded` constant in Types.lean (value: 1000000.0)
- Replaced magic number usages in `collectFlexItems` with `Length.unbounded`
- Clear documentation explaining the constant's purpose

**Files Changed:**
- `Trellis/Types.lean` - Added `Length.unbounded` constant
- `Trellis/Algorithm.lean` - Replaced magic numbers

---

### [COMPLETED] Iterative Flex Resolution for Constraint Compliance
**Status:** ✅ Implemented

**What was done:**
- Rewrote `distributeGrowth` with iterative constraint resolution loop
- Rewrote `distributeShrinkage` with iterative constraint resolution loop
- Items that hit min/max constraints are frozen, remaining space redistributed to unfrozen items
- Added helper functions: `unfrozenFlexGrow`, `unfrozenFlexShrinkScaled`, `calculateFreeSpace`
- Added 5 comprehensive tests for constraint edge cases

**Files Changed:**
- `Trellis/Algorithm.lean` - Iterative grow/shrink algorithms
- `TrellisTests/Main.lean` - Added constraint resolution test suite

---

### [COMPLETED] Separate Flexbox and Grid into Distinct Modules
**Status:** ✅ Implemented

**What was done:**
- Split 1143-line Algorithm.lean into focused modules
- `FlexAlgorithm.lean` (502 lines): All flexbox types and layout functions
- `GridAlgorithm.lean` (575 lines): All grid types and layout functions
- `Algorithm.lean` (97 lines): Entry point, shared utilities, recursive layout

**Files Changed:**
- `Trellis/Algorithm.lean` - Simplified to entry point
- `Trellis/FlexAlgorithm.lean` - New file with flexbox logic
- `Trellis/GridAlgorithm.lean` - New file with grid logic

---

### [Priority: Medium] Use StateM Monad for Mutable Layout State
**Current State:** Layout algorithms use `Id.run do` with explicit `let mut` patterns for mutable state.

**Proposed Change:** Consider using `StateM` or `StateT` for cleaner accumulation patterns, especially in grid auto-placement and track sizing.

**Benefits:** More idiomatic Lean 4 code, easier to compose with other effects if needed.

**Affected Files:**
- `/Users/Shared/Projects/lean-workspace/trellis/Trellis/Algorithm.lean`

**Estimated Effort:** Medium

---

### [Priority: Medium] Type-Safe Node IDs
**Current State:** Node IDs are raw `Nat` values that can be confused with other numeric values.

**Proposed Change:** Create an opaque `NodeId` type to prevent accidental misuse of numeric values as node identifiers.

**Benefits:** Compile-time prevention of ID misuse, clearer API contracts.

**Affected Files:**
- `/Users/Shared/Projects/lean-workspace/trellis/Trellis/Node.lean` (lines 78-85)
- `/Users/Shared/Projects/lean-workspace/trellis/Trellis/Result.lean`
- `/Users/Shared/Projects/lean-workspace/trellis/Trellis/Algorithm.lean`

**Estimated Effort:** Small

---

### [Priority: Medium] Improve LayoutResult Lookup Performance
**Current State:** `LayoutResult.get` uses `Array.find?` which is O(n) lookup (line 107-108).

**Proposed Change:** Use a `HashMap NodeId ComputedLayout` or maintain sorted array for binary search.

**Benefits:** O(1) or O(log n) lookups instead of O(n) for large layout trees.

**Affected Files:**
- `/Users/Shared/Projects/lean-workspace/trellis/Trellis/Result.lean` (lines 98-141)

**Estimated Effort:** Small

---

### [COMPLETED] Proper Baseline Alignment
**Status:** ✅ Implemented

**What was done:**
- Added `baseline` field to `ContentSize` structure with helpers `withBaseline` and `getBaseline`
- Added `baseline` field to `FlexItemState` and `FlexLine.maxBaseline` for flex layout
- Added `baseline` field to `GridItemState` for grid layout
- Implemented `computeLineMaxBaseline` and `computeLineCrossSizeWithBaseline` helpers
- Updated `computeCrossPositions` to position flex items by baseline alignment
- Added `computeRowBaselines` for grid row baseline calculation
- Updated `alignInCell` to handle baseline alignment with row baselines
- Added 6 comprehensive tests for baseline alignment (flex and grid)

**Files Changed:**
- `Trellis/Node.lean` - Added baseline to ContentSize
- `Trellis/FlexAlgorithm.lean` - Added baseline tracking and alignment
- `Trellis/GridAlgorithm.lean` - Added baseline tracking and alignment
- `TrellisTests/Main.lean` - Added baseline alignment test suite

---

### [Priority: Low] Remove Inhabited Instance Boilerplate
**Current State:** Multiple structures define default values both as an `Inhabited` instance and as explicit `default`/`empty`/`zero` functions.

**Proposed Change:** Use `deriving Inhabited` with `@[default_instance]` where possible, or consolidate to a single source of truth.

**Benefits:** Less redundant code, single source of truth for defaults.

**Affected Files:**
- `/Users/Shared/Projects/lean-workspace/trellis/Trellis/Flex.lean` (lines 79, 109)
- `/Users/Shared/Projects/lean-workspace/trellis/Trellis/Grid.lean` (lines 51, 144)
- `/Users/Shared/Projects/lean-workspace/trellis/Trellis/Types.lean`

**Estimated Effort:** Small

---

### [Priority: Low] Use `partial` Strategically with Termination Proofs
**Current State:** `layoutNode` is marked `partial` without a termination proof (line 891). Similarly, `nodeCount` and `allIds` in Node.lean (lines 221-226).

**Proposed Change:** Add `termination_by` clauses or refactor to use `Nat.rec` patterns to prove termination.

**Benefits:** Stronger correctness guarantees, avoidance of potential infinite loops.

**Affected Files:**
- `/Users/Shared/Projects/lean-workspace/trellis/Trellis/Algorithm.lean` (line 891)
- `/Users/Shared/Projects/lean-workspace/trellis/Trellis/Node.lean` (lines 221-226)

**Estimated Effort:** Medium

---

### [Priority: Low] Add `@[inline]` Annotations for Performance-Critical Functions
**Current State:** Many small helper functions like `AxisInfo.mainSize`, `EdgeInsets.horizontal`, etc. are not annotated for inlining.

**Proposed Change:** Add `@[inline]` or `@[always_inline]` to frequently-called small functions.

**Benefits:** Better runtime performance by avoiding function call overhead.

**Affected Files:**
- `/Users/Shared/Projects/lean-workspace/trellis/Trellis/Axis.lean`
- `/Users/Shared/Projects/lean-workspace/trellis/Trellis/Types.lean`

**Estimated Effort:** Small

---

## Code Cleanup

### [Priority: High] Unused Parameter Warning in `autoPlaceItem`
**Issue:** The `_flow` parameter in `autoPlaceItem` (line 649) is prefixed with underscore indicating it's unused, but `GridAutoFlow` should affect placement behavior (row vs column major, dense packing).

**Location:** `/Users/Shared/Projects/lean-workspace/trellis/Trellis/Algorithm.lean`, line 649

**Action Required:** Either implement flow-aware placement or document why it's not implemented yet.

**Estimated Effort:** Small (for TODO comment) or Medium (for implementation)

---

### [Priority: Medium] Consistent Error Handling in `get!` Functions
**Issue:** `LayoutResult.get!` uses `panic!` (line 113-114) which is not recoverable. Consider returning `Option` or using an error monad.

**Location:** `/Users/Shared/Projects/lean-workspace/trellis/Trellis/Result.lean`, lines 111-114

**Action Required:** Either remove the `get!` function, rename to make the panic clearer (e.g., `getOrPanic`), or convert to a proper error type.

**Estimated Effort:** Small

---

### [Priority: Medium] Add Documentation Comments to Public API
**Issue:** While there are some doc comments, many public functions lack comprehensive documentation. For example, the main `layout` function (line 955-956) has no doc comment explaining parameters and return value.

**Location:** Throughout all files, particularly:
- `/Users/Shared/Projects/lean-workspace/trellis/Trellis/Algorithm.lean`
- `/Users/Shared/Projects/lean-workspace/trellis/Trellis/Node.lean`

**Action Required:** Add `/-!` section comments and `/--` doc comments to all public functions.

**Estimated Effort:** Medium

---

### [Priority: Medium] Consolidate Test Helper Functions
**Issue:** `floatNear` and `shouldBeNear` in tests duplicate functionality that could be in the test framework (Crucible).

**Location:** `/Users/Shared/Projects/lean-workspace/trellis/TrellisTests/Main.lean`, lines 16-22

**Action Required:** Either move to Crucible as reusable assertions or keep but add to a shared test utilities module.

**Estimated Effort:** Small

---

### [Priority: Low] Remove Redundant `namespace`/`end` Pairs
**Issue:** Some small namespaces could be combined or simplified. For example, `ContainerKind` and `ItemKind` in Node.lean have very few members each.

**Location:** `/Users/Shared/Projects/lean-workspace/trellis/Trellis/Node.lean`

**Action Required:** Consider combining related types into a single namespace or using dot notation directly.

**Estimated Effort:** Small

---

### [Priority: Low] Standardize Naming Conventions
**Issue:** Some inconsistencies in naming:
- `finish` vs `end` (GridSpan uses `finish` to avoid keyword, but could use backticks)
- `mk'` constructors vs named constructors
- `fromSizes` vs `pixels` (different patterns for similar functionality)

**Location:**
- `/Users/Shared/Projects/lean-workspace/trellis/Trellis/Grid.lean` (line 78, `finish`)
- `/Users/Shared/Projects/lean-workspace/trellis/Trellis/Result.lean` (line 21, `mk'`)
- `/Users/Shared/Projects/lean-workspace/trellis/Trellis/Grid.lean` (lines 53-63)

**Action Required:** Establish and document naming conventions, then apply consistently.

**Estimated Effort:** Small

---

### [Priority: Low] Add Property-Based Tests
**Issue:** Current tests are example-based. Property-based tests would provide better coverage for edge cases.

**Location:** `/Users/Shared/Projects/lean-workspace/trellis/TrellisTests/Main.lean`

**Action Required:** Add property-based tests using plausible or similar library. Examples:
- "Total width of flex items equals container width when all items have grow > 0"
- "Grid items never overlap"
- "Layout positions are always non-negative"

**Estimated Effort:** Medium

**Dependencies:** Property-based testing library (plausible)

---

### [Priority: Low] Clean Up Import Structure
**Issue:** `Algorithm.lean` imports all other modules but some imports may be transitively satisfied.

**Location:** `/Users/Shared/Projects/lean-workspace/trellis/Trellis/Algorithm.lean`, lines 5-10

**Action Required:** Review import graph and minimize direct imports to only what's directly needed.

**Estimated Effort:** Small

---

## Testing Improvements

### [Priority: High] Add Tests for Edge Cases
**Issue:** Missing test coverage for:
- Negative margins
- Zero-width/height containers
- Very large numbers of children
- Deeply nested containers
- Mixed flex/grid nesting

**Location:** `/Users/Shared/Projects/lean-workspace/trellis/TrellisTests/Main.lean`

**Action Required:** Add comprehensive edge case test suite.

**Estimated Effort:** Medium

---

### [Priority: Medium] Add Performance Benchmarks
**Issue:** No benchmarks to track layout performance over time.

**Location:** New file(s) in TrellisTests

**Action Required:** Create benchmark suite measuring layout time for various tree sizes and complexities.

**Estimated Effort:** Medium

---

### [Priority: Medium] Add Visual Regression Tests
**Issue:** It's hard to verify that layout results "look correct" without visual output.

**Location:** New test infrastructure

**Action Required:** Create ASCII art or image-based visual tests that render layouts for visual verification.

**Estimated Effort:** Medium

---

## Architecture Considerations

### Dependency Injection for Content Measurement
**Current State:** `getContentSize` only looks at the `content` field of nodes. Real applications need to measure text and other dynamic content.

**Recommendation:** Define a `ContentMeasurer` typeclass or pass a measurement function to `layout`. This aligns with how arbor handles text measurement.

### Integration with Widget Systems
**Current State:** Trellis is used by arbor and afferent. The integration point is primarily through `LayoutNode` construction.

**Recommendation:** Consider a builder API or DSL to make tree construction more ergonomic:
```lean
layout do
  row (gap := 10) {
    leaf (width := 100, height := 50)
    column {
      leaf (flexGrow := 1)
      leaf (flexGrow := 2)
    }
  }
```

### Consider CSS Variables/Custom Properties
**Long-term:** For theming support, consider how CSS custom properties could be represented and resolved during layout.

---

## Summary

| Category | High | Medium | Low | Completed |
|----------|------|--------|-----|-----------|
| Features | 0 | 5 | 3 | 3 |
| Improvements | 0 | 3 | 3 | 4 |
| Cleanup | 2 | 3 | 4 | 0 |
| Testing | 1 | 2 | 0 | 0 |

**Recently Completed:**
- ✅ Proper baseline alignment for flex and grid layouts
- ✅ Split Algorithm.lean into FlexAlgorithm.lean and GridAlgorithm.lean
- ✅ Replace magic numbers with `Length.unbounded` constant
- ✅ CSS Flexbox iterative constraint resolution (grow/shrink with min/max freezing)
- ✅ CSS Grid `repeat()` function support (count, auto-fill, auto-fit)
- ✅ CSS Grid `minmax()` track sizing (full min/max clamping and fr distribution)
- ✅ CSS Flexbox `flex-wrap` multi-line layout improvements (wrap-reverse fix)

**Key Priorities:**
1. Implement CSS Grid named lines and areas
2. Add Flexbox `order` property support
3. Add `aspect-ratio` property support
4. Use StateM monad for mutable layout state
