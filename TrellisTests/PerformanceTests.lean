/-
  Performance / Stress Tests for Trellis CSS Layout Library

  Tests large and complex layout trees to ensure the implementation
  handles extreme scale without crashing or hanging.
-/

import Crucible
import Trellis
import Chronos

namespace TrellisTests.PerformanceTests

open Crucible
open Trellis

testSuite "Performance Tests"

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

/-! ## Deep Nesting Tests -/

test "perf: 100-level deep flex column" := do
  let node := buildDeepColumn 100
  let start ← Chronos.MonotonicTime.now
  let result := layout node 500 10000
  let elapsed ← start.elapsed
  -- 100 containers + 1 leaf = 101 nodes
  shouldBe result.layouts.size 101
  IO.println s!"  [100-level deep column: {elapsed}]"

test "perf: 500-level deep flex column" := do
  let node := buildDeepColumn 500
  let start ← Chronos.MonotonicTime.now
  let result := layout node 500 50000
  let elapsed ← start.elapsed
  shouldBe result.layouts.size 501
  IO.println s!"  [500-level deep column: {elapsed}]"

test "perf: 1000-level deep flex column" := do
  let node := buildDeepColumn 1000
  let start ← Chronos.MonotonicTime.now
  let result := layout node 500 100000
  let elapsed ← start.elapsed
  shouldBe result.layouts.size 1001
  IO.println s!"  [1000-level deep column: {elapsed}]"

/-! ## Wide Fan-Out Tests -/

test "perf: 100 flex items in row" := do
  let node := buildWideRow 100
  let start ← Chronos.MonotonicTime.now
  let result := layout node 10000 100
  let elapsed ← start.elapsed
  -- 1 container + 100 leaves = 101 nodes
  shouldBe result.layouts.size 101
  IO.println s!"  [100 flex items in row: {elapsed}]"

test "perf: 1000 flex items in row" := do
  let node := buildWideRow 1000
  let start ← Chronos.MonotonicTime.now
  let result := layout node 100000 100
  let elapsed ← start.elapsed
  shouldBe result.layouts.size 1001
  IO.println s!"  [1000 flex items in row: {elapsed}]"

test "perf: 5000 flex items in row" := do
  let node := buildWideRow 5000
  let start ← Chronos.MonotonicTime.now
  let result := layout node 500000 100
  let elapsed ← start.elapsed
  shouldBe result.layouts.size 5001
  IO.println s!"  [5000 flex items in row: {elapsed}]"

/-! ## Flex Wrap Tests -/

test "perf: 1000 items wrapping (10 items per line)" := do
  -- Items are 50px wide with 5px gap, in 500px container
  -- ~9 items per line, ~111 lines
  let node := buildWrappingRow 1000 50
  let start ← Chronos.MonotonicTime.now
  let result := layout node 500 50000
  let elapsed ← start.elapsed
  shouldBe result.layouts.size 1001
  IO.println s!"  [1000 items wrapping: {elapsed}]"

test "perf: 5000 items wrapping (50 items per line)" := do
  -- Items are 20px wide with 5px gap, in 1200px container
  -- ~48 items per line, ~104 lines
  let node := buildWrappingRow 5000 20
  let start ← Chronos.MonotonicTime.now
  let result := layout node 1200 50000
  let elapsed ← start.elapsed
  shouldBe result.layouts.size 5001
  IO.println s!"  [5000 items wrapping: {elapsed}]"

/-! ## Flex Shrinkage Tests -/

test "perf: 100 items with shrinkage overflow" := do
  -- 100 items x 50px = 5000px content in 1000px container
  let node := buildShrinkingRow 100 50
  let start ← Chronos.MonotonicTime.now
  let result := layout node 1000 100
  let elapsed ← start.elapsed
  shouldBe result.layouts.size 101
  IO.println s!"  [100 items shrinking: {elapsed}]"

test "perf: 500 items with shrinkage overflow" := do
  -- 500 items x 30px = 15000px content in 2000px container
  let node := buildShrinkingRow 500 30
  let start ← Chronos.MonotonicTime.now
  let result := layout node 2000 100
  let elapsed ← start.elapsed
  shouldBe result.layouts.size 501
  IO.println s!"  [500 items shrinking: {elapsed}]"

/-! ## Grid Layout Tests -/

test "perf: 10x10 grid (100 cells)" := do
  let node := buildGrid 10 10
  let start ← Chronos.MonotonicTime.now
  let result := layout node 500 500
  let elapsed ← start.elapsed
  shouldBe result.layouts.size 101
  IO.println s!"  [10x10 grid: {elapsed}]"

test "perf: 50x50 grid (2500 cells)" := do
  let node := buildGrid 50 50
  let start ← Chronos.MonotonicTime.now
  let result := layout node 2000 2000
  let elapsed ← start.elapsed
  shouldBe result.layouts.size 2501
  IO.println s!"  [50x50 grid: {elapsed}]"

test "perf: 100x100 grid (10000 cells)" := do
  let node := buildGrid 100 100
  let start ← Chronos.MonotonicTime.now
  let result := layout node 4000 4000
  let elapsed ← start.elapsed
  shouldBe result.layouts.size 10001
  IO.println s!"  [100x100 grid: {elapsed}]"

/-! ## Mixed Layout Tests -/

test "perf: grid of 100 flex containers with 50 items each" := do
  -- 10 columns x 100 flex containers, each with 50 items
  -- Total: 1 grid + 100 flex + 5000 leaves = 5101 nodes
  let node := buildGridOfFlexContainers 10 100 50
  let start ← Chronos.MonotonicTime.now
  let result := layout node 2000 5000
  let elapsed ← start.elapsed
  shouldBe result.layouts.size 5101
  IO.println s!"  [grid of 100 flex containers x 50 items: {elapsed}]"

test "perf: 10-level alternating flex/grid nesting" := do
  let node := buildAlternatingNested 10 5
  let start ← Chronos.MonotonicTime.now
  let result := layout node 1000 1000
  let elapsed ← start.elapsed
  -- Verify layout completes and produces reasonable result
  shouldSatisfy (result.layouts.size > 10) "should have multiple layouts"
  IO.println s!"  [10-level alternating nested: {elapsed}]"

#generate_tests

end TrellisTests.PerformanceTests
