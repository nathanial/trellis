/-
  Performance / Stress Tests for Trellis CSS Layout Library

  EXTREME EDITION - Tests massive layout trees to ensure the implementation
  handles extreme scale without crashing or hanging.
-/

import Crucible
import Trellis
import Chronos

namespace TrellisTests.PerformanceTests

open Crucible
open Trellis

testSuite "Performance Tests"

/-- Force strict evaluation of a pure computation by storing in a ref and reading back.
    This defeats Lean's lazy evaluation so we can time pure computations. -/
@[inline] def strictEval {α : Type} (x : α) : IO α := do
  let ref ← IO.mkRef x
  ref.get

/-! ## Helper Functions -/

/-- Build a deeply nested flex column where each level has one child.
    The innermost node is a leaf with content size 10x10.
    Total nodes = depth + 1 (depth containers + 1 leaf). -/
def buildDeepColumn (depth : Nat) : LayoutNode := Id.run do
  -- Start with innermost leaf
  let mut current := LayoutNode.leaf' (depth) 10 10
  -- Wrap in column containers from inside out
  for i in [:depth] do
    let id := depth - 1 - i
    current := LayoutNode.column id #[current]
  return current

/-- Build a wide flex row with N leaf children.
    Each child is a 10x10 leaf. Total nodes = width + 1 (1 container + N leaves). -/
def buildWideRow (width : Nat) : LayoutNode := Id.run do
  let mut children : Array LayoutNode := #[]
  for i in [:width] do
    children := children.push (LayoutNode.leaf' (i + 1) 10 10)
  return LayoutNode.row 0 children

/-- Build a wrapping flex row with N items.
    Each item has the given width. Container width determines wrap behavior. -/
def buildWrappingRow (itemCount : Nat) (itemWidth : Float) : LayoutNode := Id.run do
  let mut children : Array LayoutNode := #[]
  for i in [:itemCount] do
    children := children.push (LayoutNode.leaf' (i + 1) itemWidth 20)
  return LayoutNode.flexBox 0 (FlexContainer.rowWrap 5 5) children

/-- Build a flex row with N items that have shrink factors.
    Total content width exceeds container, forcing shrinkage. -/
def buildShrinkingRow (itemCount : Nat) (itemWidth : Float) : LayoutNode := Id.run do
  let mut children : Array LayoutNode := #[]
  for i in [:itemCount] do
    -- Varying shrink factors from 0.5 to 2.0
    let shrinkFactor := 0.5 + (i % 4).toFloat * 0.5
    let item := LayoutNode.leaf' (i + 1) itemWidth 30 {}
      (.flexChild { shrink := shrinkFactor, basis := .length itemWidth })
    children := children.push item
  return LayoutNode.row 0 children

/-- Build a grid with rows x cols cells, all auto-placed. -/
def buildGrid (rows cols : Nat) : LayoutNode := Id.run do
  let itemCount := rows * cols
  let mut children : Array LayoutNode := #[]
  for i in [:itemCount] do
    children := children.push (LayoutNode.leaf' (i + 1) 20 20)
  return LayoutNode.gridBox 0 (GridContainer.columns cols 5) children

/-- Build a grid containing flex containers, each with multiple items. -/
def buildGridOfFlexContainers (gridCols flexContainerCount itemsPerContainer : Nat) : LayoutNode := Id.run do
  let mut containers : Array LayoutNode := #[]
  let mut nodeId := 1
  for c in [:flexContainerCount] do
    let mut items : Array LayoutNode := #[]
    for _ in [:itemsPerContainer] do
      items := items.push (LayoutNode.leaf' nodeId 15 15)
      nodeId := nodeId + 1
    -- Use IDs after leaves to avoid collision
    let containerId := flexContainerCount * itemsPerContainer + 1 + c
    let container := LayoutNode.row containerId items (gap := 2)
    containers := containers.push container
  return LayoutNode.gridBox 0 (GridContainer.columns gridCols 10) containers

/-- Build alternating nested flex/grid containers. -/
def buildAlternatingNested (depth itemsPerLevel : Nat) : LayoutNode := Id.run do
  let mut nodeId := depth * (itemsPerLevel + 1)
  -- Build innermost leaves
  let mut innerItems : Array LayoutNode := #[]
  for _ in [:itemsPerLevel] do
    innerItems := innerItems.push (LayoutNode.leaf' nodeId 10 10)
    nodeId := nodeId + 1
  -- Start with innermost container
  let mut current := LayoutNode.row (depth) innerItems
  -- Alternate grid/flex wrapping
  for i in [:depth] do
    let level := depth - 1 - i
    let mut levelItems : Array LayoutNode := #[current]
    for _ in [:itemsPerLevel - 1] do
      levelItems := levelItems.push (LayoutNode.leaf' nodeId 10 10)
      nodeId := nodeId + 1
    if i % 2 == 0 then
      -- Grid layer
      current := LayoutNode.gridBox level (GridContainer.columns 3 5) levelItems
    else
      -- Flex layer
      current := LayoutNode.row level levelItems (gap := 5)
  return current

/-- Build a tree with fan-out at each level (exponential growth).
    Each non-leaf node has `fanOut` children. -/
def buildFanOutTree (depth fanOut : Nat) : LayoutNode := Id.run do
  let mut nodeId := 0
  let mut currentLevel : Array LayoutNode := #[]
  -- Start with leaves at the bottom
  let numLeaves := fanOut ^ depth
  for _ in [:numLeaves] do
    currentLevel := currentLevel.push (LayoutNode.leaf' nodeId 10 10)
    nodeId := nodeId + 1
  -- Build up through the levels
  for _ in [:depth] do
    let mut nextLevel : Array LayoutNode := #[]
    let numContainers := currentLevel.size / fanOut
    for c in [:numContainers] do
      let start := c * fanOut
      let children := currentLevel.extract start (start + fanOut)
      nextLevel := nextLevel.push (LayoutNode.row nodeId children (gap := 2))
      nodeId := nodeId + 1
    currentLevel := nextLevel
  return currentLevel[0]!

/-! ## Deep Nesting Tests (now unlimited depth with iterative algorithm) -/

test "perf: 1000-level deep flex column" := do
  let node := buildDeepColumn 1000
  let start ← Chronos.MonotonicTime.now
  let result ← strictEval (layout node 500 100000)
  let elapsed ← start.elapsed
  shouldBe result.layouts.size 1001
  IO.println s!"  [1000-level deep column: {elapsed}]"

test "perf: 5000-level deep flex column" := do
  let node := buildDeepColumn 5000
  let start ← Chronos.MonotonicTime.now
  let result ← strictEval (layout node 500 500000)
  let elapsed ← start.elapsed
  shouldBe result.layouts.size 5001
  IO.println s!"  [5000-level deep column: {elapsed}]"

test "perf: 10000-level deep flex column" := do
  let node := buildDeepColumn 10000
  let start ← Chronos.MonotonicTime.now
  let result ← strictEval (layout node 500 1000000)
  let elapsed ← start.elapsed
  shouldBe result.layouts.size 10001
  IO.println s!"  [10000-level deep column: {elapsed}]"

test "perf: 50000-level deep flex column" := do
  let node := buildDeepColumn 50000
  let start ← Chronos.MonotonicTime.now
  let result ← strictEval (layout node 500 5000000)
  let elapsed ← start.elapsed
  shouldBe result.layouts.size 50001
  IO.println s!"  [50000-level deep column: {elapsed}]"

test "perf: 100000-level deep flex column" := do
  let node := buildDeepColumn 100000
  let start ← Chronos.MonotonicTime.now
  let result ← strictEval (layout node 500 10000000)
  let elapsed ← start.elapsed
  shouldBe result.layouts.size 100001
  IO.println s!"  [100000-level deep column: {elapsed}]"

/-! ## Wide Fan-Out Tests (100x scale) -/

test "perf: 10000 flex items in row" := do
  let node := buildWideRow 10000
  let start ← Chronos.MonotonicTime.now
  let result ← strictEval (layout node 1000000 100)
  let elapsed ← start.elapsed
  shouldBe result.layouts.size 10001
  IO.println s!"  [10000 flex items in row: {elapsed}]"

test "perf: 100000 flex items in row" := do
  let node := buildWideRow 100000
  let start ← Chronos.MonotonicTime.now
  let result ← strictEval (layout node 10000000 100)
  let elapsed ← start.elapsed
  shouldBe result.layouts.size 100001
  IO.println s!"  [100000 flex items in row: {elapsed}]"

test "perf: 500000 flex items in row" := do
  let node := buildWideRow 500000
  let start ← Chronos.MonotonicTime.now
  let result ← strictEval (layout node 50000000 100)
  let elapsed ← start.elapsed
  shouldBe result.layouts.size 500001
  IO.println s!"  [500000 flex items in row: {elapsed}]"

/-! ## Flex Wrap Tests (100x scale) -/

test "perf: 100000 items wrapping" := do
  -- Items are 50px wide with 5px gap, in 5000px container
  -- ~91 items per line, ~1099 lines
  let node := buildWrappingRow 100000 50
  let start ← Chronos.MonotonicTime.now
  let result ← strictEval (layout node 5000 5000000)
  let elapsed ← start.elapsed
  shouldBe result.layouts.size 100001
  IO.println s!"  [100000 items wrapping: {elapsed}]"

test "perf: 500000 items wrapping" := do
  -- Items are 20px wide with 5px gap, in 12000px container
  -- ~480 items per line, ~1042 lines
  let node := buildWrappingRow 500000 20
  let start ← Chronos.MonotonicTime.now
  let result ← strictEval (layout node 12000 5000000)
  let elapsed ← start.elapsed
  shouldBe result.layouts.size 500001
  IO.println s!"  [500000 items wrapping: {elapsed}]"

/-! ## Flex Shrinkage Tests (100x scale) -/

test "perf: 10000 items with shrinkage overflow" := do
  -- 10000 items x 50px = 500000px content in 10000px container
  let node := buildShrinkingRow 10000 50
  let start ← Chronos.MonotonicTime.now
  let result ← strictEval (layout node 10000 100)
  let elapsed ← start.elapsed
  shouldBe result.layouts.size 10001
  IO.println s!"  [10000 items shrinking: {elapsed}]"

test "perf: 50000 items with shrinkage overflow" := do
  -- 50000 items x 30px = 1500000px content in 20000px container
  let node := buildShrinkingRow 50000 30
  let start ← Chronos.MonotonicTime.now
  let result ← strictEval (layout node 20000 100)
  let elapsed ← start.elapsed
  shouldBe result.layouts.size 50001
  IO.println s!"  [50000 items shrinking: {elapsed}]"

/-! ## Grid Layout Tests (100x scale) -/

test "perf: 100x100 grid (10000 cells)" := do
  let node := buildGrid 100 100
  let start ← Chronos.MonotonicTime.now
  let result ← strictEval (layout node 5000 5000)
  let elapsed ← start.elapsed
  shouldBe result.layouts.size 10001
  IO.println s!"  [100x100 grid: {elapsed}]"

test "perf: 500x500 grid (250000 cells)" := do
  let node := buildGrid 500 500
  let start ← Chronos.MonotonicTime.now
  let result ← strictEval (layout node 20000 20000)
  let elapsed ← start.elapsed
  shouldBe result.layouts.size 250001
  IO.println s!"  [500x500 grid: {elapsed}]"

test "perf: 1000x1000 grid (1000000 cells)" := do
  let node := buildGrid 1000 1000
  let start ← Chronos.MonotonicTime.now
  let result ← strictEval (layout node 40000 40000)
  let elapsed ← start.elapsed
  shouldBe result.layouts.size 1000001
  IO.println s!"  [1000x1000 grid (1M cells): {elapsed}]"

/-! ## Mixed Layout Tests (100x scale) -/

test "perf: grid of 1000 flex containers with 500 items each" := do
  -- 20 columns x 1000 flex containers, each with 500 items
  -- Total: 1 grid + 1000 flex + 500000 leaves = 501001 nodes
  let node := buildGridOfFlexContainers 20 1000 500
  let start ← Chronos.MonotonicTime.now
  let result ← strictEval (layout node 20000 500000)
  let elapsed ← start.elapsed
  shouldBe result.layouts.size 501001
  IO.println s!"  [grid of 1000 flex containers x 500 items: {elapsed}]"

test "perf: 50-level alternating flex/grid nesting" := do
  let node := buildAlternatingNested 50 10
  let start ← Chronos.MonotonicTime.now
  let result ← strictEval (layout node 10000 10000)
  let elapsed ← start.elapsed
  -- Verify layout completes and produces reasonable result
  shouldSatisfy (result.layouts.size > 50) "should have multiple layouts"
  IO.println s!"  [50-level alternating nested: {elapsed}]"

/-! ## Exponential Fan-Out Tests -/

test "perf: fan-out tree depth=8 fanout=4 (87K+ nodes)" := do
  -- 4^8 = 65,536 leaves, plus internal nodes
  -- Total nodes = sum of 4^i for i=0..8 = (4^9 - 1) / 3 = 87,381
  let node := buildFanOutTree 8 4
  let start ← Chronos.MonotonicTime.now
  let result ← strictEval (layout node 10000 10000)
  let elapsed ← start.elapsed
  shouldSatisfy (result.layouts.size > 80000) "should have over 80K layouts"
  IO.println s!"  [fan-out tree 4^8: {elapsed}]"

test "perf: fan-out tree depth=10 fanout=4 (1.3M+ nodes)" := do
  -- 4^10 = 1,048,576 leaves, plus internal nodes
  -- Total nodes = sum of 4^i for i=0..10 = (4^11 - 1) / 3 = 1,398,101
  let node := buildFanOutTree 10 4
  let start ← Chronos.MonotonicTime.now
  let result ← strictEval (layout node 100000 100000)
  let elapsed ← start.elapsed
  shouldSatisfy (result.layouts.size > 1000000) "should have over 1M layouts"
  IO.println s!"  [fan-out tree 4^10: {elapsed}]"



end TrellisTests.PerformanceTests
