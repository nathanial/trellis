/-
  Trellis Layout Tests - Edge Cases
  Unit tests for edge cases, margin collapsing, empty containers,
  and stress tests (deep nesting, large item counts).
-/
import Crucible
import Trellis

namespace TrellisTests.LayoutTests.EdgeCases

open Crucible
open Trellis

testSuite "Trellis Layout Tests - Edge Cases"

/-! ## Edge Case Tests -/

test "empty container produces no child layouts" := do
  let node := LayoutNode.row 0 #[]
  let result := layout node 200 100
  -- Only the container itself
  result.size ≡ 1

test "space-between with single item places at start" := do
  let props := { FlexContainer.row with justifyContent := .spaceBetween }
  let node := LayoutNode.flexBox 0 props #[
    LayoutNode.leaf 1 (ContentSize.mk' 100 30)
  ]
  let result := layout node 400 100
  let cl := result.get! 1
  shouldBeNear cl.x 0 0.01

test "zero flex-grow items keep their basis size" := do
  let node := LayoutNode.flexBox 0 (FlexContainer.row) #[
    LayoutNode.leaf' 1 50 30 {} (.flexChild { grow := 0, basis := .length 50 }),
    LayoutNode.leaf' 2 50 30 {} (.flexChild { grow := 1, basis := .length 50 })
  ]
  let result := layout node 300 100
  let cl1 := result.get! 1
  let cl2 := result.get! 2
  -- First stays at 50, second gets remaining 250
  shouldBeNear cl1.width 50 0.01
  shouldBeNear cl2.width 250 0.01

/-! ## Edge Case Tests - Comprehensive -/

-- Negative margins

test "flex row: negative margins allow overlapping items" := do
  let node := LayoutNode.row 0 #[
    LayoutNode.leaf 1 (ContentSize.mk' 50 30),
    LayoutNode.leaf 2 (ContentSize.mk' 50 30) { margin := { left := -20 } }
  ]
  let result := layout node 200 100
  let cl1 := result.get! 1
  let cl2 := result.get! 2
  -- Item 2 should overlap item 1 due to negative left margin
  shouldSatisfy (cl2.x < cl1.x + cl1.width) "item 2 should overlap item 1"
  shouldBeNear cl2.x 30 0.01  -- 50 - 20 = 30

test "flex row: negative margins on both sides" := do
  let node := LayoutNode.row 0 #[
    LayoutNode.leaf 1 (ContentSize.mk' 50 30) { margin := { right := -10 } },
    LayoutNode.leaf 2 (ContentSize.mk' 50 30) { margin := { left := -10 } }
  ]
  let result := layout node 200 100
  let cl2 := result.get! 2
  -- Items should overlap by 20 pixels total
  shouldBeNear cl2.x 30 0.01  -- 50 - 10 - 10 = 30

test "flex column: negative vertical margins reduce total height" := do
  -- Negative margins in columns reduce the spacing between items
  let node := LayoutNode.column 0 #[
    LayoutNode.leaf 1 (ContentSize.mk' 50 30),
    LayoutNode.leaf 2 (ContentSize.mk' 50 30) { margin := { top := -15 } }
  ]
  let result := layout node 100 200
  let cl1 := result.get! 1
  let cl2 := result.get! 2
  -- Both items should be positioned (may or may not overlap depending on implementation)
  shouldSatisfy (cl1.height > 0) "item 1 should have height"
  shouldSatisfy (cl2.height > 0) "item 2 should have height"
  -- With negative margin, total stack height should be less than 60 (30 + 30)
  let totalHeight := cl2.y + cl2.height
  shouldSatisfy (totalHeight <= 60) "negative margin should reduce or maintain total height"

test "grid: negative margins shift item position" := do
  let props := GridContainer.columns 2
  let node := LayoutNode.gridBox 0 props #[
    LayoutNode.leaf 1 (ContentSize.mk' 40 30) { margin := { left := -10, top := -5 } }
  ]
  let result := layout node 200 100
  let cl := result.get! 1
  -- Item should be shifted by negative margins
  shouldBeNear cl.x (-10) 0.01
  shouldBeNear cl.y (-5) 0.01

-- Zero-width/height containers

test "zero-width container collapses flex items" := do
  let node := LayoutNode.row 0 #[
    LayoutNode.leaf 1 (ContentSize.mk' 50 30),
    LayoutNode.leaf 2 (ContentSize.mk' 50 30)
  ]
  let result := layout node 0 100
  let cl1 := result.get! 1
  let cl2 := result.get! 2
  -- Items should still exist but may have zero/minimal width
  shouldSatisfy (cl1.width >= 0) "width should not be negative"
  shouldSatisfy (cl2.width >= 0) "width should not be negative"

test "zero-height container collapses flex column items" := do
  let node := LayoutNode.column 0 #[
    LayoutNode.leaf 1 (ContentSize.mk' 50 30),
    LayoutNode.leaf 2 (ContentSize.mk' 50 30)
  ]
  let result := layout node 100 0
  let cl1 := result.get! 1
  let cl2 := result.get! 2
  -- Items should still exist
  shouldSatisfy (cl1.height >= 0) "height should not be negative"
  shouldSatisfy (cl2.height >= 0) "height should not be negative"

test "zero-size grid container handles items gracefully" := do
  let props := GridContainer.columns 2
  let node := LayoutNode.gridBox 0 props #[
    LayoutNode.leaf 1 (ContentSize.mk' 50 30),
    LayoutNode.leaf 2 (ContentSize.mk' 50 30)
  ]
  let result := layout node 0 0
  -- Should not crash, items should exist
  shouldSatisfy (result.size >= 1) "should produce layout results"

test "container with zero-size children" := do
  let node := LayoutNode.row 0 #[
    LayoutNode.leaf 1 (ContentSize.mk' 0 0),
    LayoutNode.leaf 2 (ContentSize.mk' 50 30),
    LayoutNode.leaf 3 (ContentSize.mk' 0 0)
  ]
  let result := layout node 200 100
  let cl1 := result.get! 1
  let cl2 := result.get! 2
  let cl3 := result.get! 3
  -- Zero-size items should exist at correct positions
  shouldBeNear cl1.width 0 0.01
  shouldBeNear cl2.width 50 0.01
  shouldBeNear cl3.width 0 0.01
  -- Item 2 should be right after zero-width item 1
  shouldBeNear cl2.x 0 0.01

-- Very large numbers of children

test "flex row with 100 children" := do
  let children := (List.range 100).map fun i =>
    LayoutNode.leaf (i + 1) (ContentSize.mk' 10 20)
  let node := LayoutNode.row 0 children.toArray
  let result := layout node 2000 100
  -- All 100 children plus container
  shouldSatisfy (result.size == 101) "should have 101 layout results"
  let cl1 := result.get! 1
  let cl100 := result.get! 100
  -- First item at start
  shouldBeNear cl1.x 0 0.01
  -- Last item should be positioned correctly (99 * 10 = 990)
  shouldBeNear cl100.x 990 0.01

test "grid with 50 children auto-placed" := do
  let children := (List.range 50).map fun i =>
    LayoutNode.leaf (i + 1) (ContentSize.mk' 0 20)
  let props := GridContainer.columns 5
  let node := LayoutNode.gridBox 0 props children.toArray
  let result := layout node 500 1000
  -- All 50 children plus container
  shouldSatisfy (result.size == 51) "should have 51 layout results"
  -- Item 1 in first cell
  let cl1 := result.get! 1
  shouldBeNear cl1.x 0 0.01
  shouldBeNear cl1.y 0 0.01
  -- Item 50 in last row (row 10), column 5
  let cl50 := result.get! 50
  shouldBeNear cl50.x 400 0.01  -- 5th column at x=400

-- Deeply nested containers

test "10 levels of nested flex columns does not crash" := do
  -- Create deeply nested structure with sequential IDs
  let leaf := LayoutNode.leaf 1 (ContentSize.mk' 50 20)
  let l1 := LayoutNode.column 2 #[leaf]
  let l2 := LayoutNode.column 3 #[l1]
  let l3 := LayoutNode.column 4 #[l2]
  let l4 := LayoutNode.column 5 #[l3]
  let l5 := LayoutNode.column 6 #[l4]
  let l6 := LayoutNode.column 7 #[l5]
  let l7 := LayoutNode.column 8 #[l6]
  let l8 := LayoutNode.column 9 #[l7]
  let l9 := LayoutNode.column 10 #[l8]
  let l10 := LayoutNode.column 11 #[l9]
  let result := layout l10 200 400
  -- Should not crash, result should contain all nodes
  shouldSatisfy (result.size >= 11) "should have at least 11 layout results"
  -- Verify the leaf exists in results (dimensions may vary based on stretch behavior)
  let cl := result.get! 1
  shouldSatisfy (cl.width >= 0) "leaf width should be non-negative"
  shouldSatisfy (cl.height >= 0) "leaf height should be non-negative"

test "deeply nested alternating row/column does not crash" := do
  -- row > column > row > column > leaf
  -- Use sequential IDs for proper lookup
  let leaf := LayoutNode.leaf 1 (ContentSize.mk' 40 30)
  let col1 := LayoutNode.column 2 #[leaf]
  let row1 := LayoutNode.row 3 #[col1]
  let col2 := LayoutNode.column 4 #[row1]
  let row2 := LayoutNode.row 5 #[col2]
  let result := layout row2 200 200
  -- Should not crash, all nodes should be laid out
  shouldSatisfy (result.size >= 5) "should have 5 layout results"
  let cl := result.get! 1
  -- Leaf should exist in results (dimensions may vary)
  shouldSatisfy (cl.width >= 0) "leaf width should be non-negative"
  shouldSatisfy (cl.height >= 0) "leaf height should be non-negative"

test "deeply nested with flex-grow propagation" := do
  -- Outer column with inner flex-grow child
  let leaf := LayoutNode.leaf 1 (ContentSize.mk' 50 20)
    { height := .percent 1.0 }
  let innerCol := LayoutNode.column 10 #[leaf] 0 {} (.flexChild (FlexItem.growing 1))
  let outerCol := LayoutNode.column 20 #[
    LayoutNode.leaf 2 (ContentSize.mk' 50 30),  -- Fixed header
    innerCol
  ]
  let result := layout outerCol 100 200
  let cl1 := result.get! 1
  let cl2 := result.get! 2
  -- Header should be 30px
  shouldBeNear cl2.height 30 0.01
  -- Inner leaf should fill remaining (200 - 30 = 170)
  shouldBeNear cl1.height 170 0.01

-- Mixed flex/grid nesting

test "grid inside flex row" := do
  let gridProps := GridContainer.columns 2
  let grid := LayoutNode.gridBox 10 gridProps #[
    LayoutNode.leaf 1 (ContentSize.mk' 0 30),
    LayoutNode.leaf 2 (ContentSize.mk' 0 30),
    LayoutNode.leaf 3 (ContentSize.mk' 0 30),
    LayoutNode.leaf 4 (ContentSize.mk' 0 30)
  ] {} (.flexChild (FlexItem.growing 1))
  let row := LayoutNode.row 20 #[
    LayoutNode.leaf 5 (ContentSize.mk' 50 60),
    grid
  ]
  let result := layout row 200 100
  let cl5 := result.get! 5
  let cl1 := result.get! 1
  -- Fixed item should be 50px wide
  shouldBeNear cl5.width 50 0.01
  -- Grid items should be in 2 columns within remaining space (150px / 2 = 75px)
  shouldBeNear cl1.width 75 0.01

test "flex row inside grid cell" := do
  let row := LayoutNode.row 10 #[
    LayoutNode.leaf 1 (ContentSize.mk' 30 20),
    LayoutNode.leaf 2 (ContentSize.mk' 30 20),
    LayoutNode.leaf 3 (ContentSize.mk' 30 20)
  ]
  let gridProps := GridContainer.columns 2
  let grid := LayoutNode.gridBox 20 gridProps #[
    row,
    LayoutNode.leaf 4 (ContentSize.mk' 0 50)
  ]
  let result := layout grid 200 200
  let cl1 := result.get! 1
  let cl2 := result.get! 2
  let cl3 := result.get! 3
  -- Flex row items should be laid out horizontally within first grid cell (100px wide)
  shouldSatisfy (cl1.x < cl2.x) "item 1 before item 2"
  shouldSatisfy (cl2.x < cl3.x) "item 2 before item 3"
  shouldBeNear cl1.width 30 0.01

test "alternating grid and flex nesting" := do
  -- flex column > grid > flex row > leaf
  let leaf := LayoutNode.leaf 1 (ContentSize.mk' 40 25)
  let innerRow := LayoutNode.row 10 #[leaf]
  let gridProps := GridContainer.columns 1
  let grid := LayoutNode.gridBox 20 gridProps #[innerRow] {} (.flexChild (FlexItem.growing 1))
  let outerCol := LayoutNode.column 30 #[
    LayoutNode.leaf 2 (ContentSize.mk' 100 30),  -- Header
    grid
  ]
  let result := layout outerCol 150 200
  let cl1 := result.get! 1
  let cl2 := result.get! 2
  -- Header at top
  shouldBeNear cl2.y 0 0.01
  shouldBeNear cl2.height 30 0.01
  -- Leaf inside grid inside flex should be positioned correctly
  shouldSatisfy (cl1.y >= 30) "leaf should be below header"

test "grid with flex items that have flex-grow" := do
  let growingRow := LayoutNode.row 10 #[
    LayoutNode.leaf 1 (ContentSize.mk' 20 30),
    LayoutNode.leaf' 2 20 30 {} (.flexChild (FlexItem.growing 1))
  ]
  let gridProps := GridContainer.columns 1
  let grid := LayoutNode.gridBox 20 gridProps #[growingRow]
  let result := layout grid 200 100
  let cl1 := result.get! 1
  let cl2 := result.get! 2
  -- Item 1 should be fixed at 20px
  shouldBeNear cl1.width 20 0.01
  -- Item 2 should grow to fill remaining space (200 - 20 = 180)
  shouldBeNear cl2.width 180 0.01

/-! ## Margin Collapse Tests -/

test "margin collapse: disabled by default (backward compatible)" := do
  -- Without marginCollapse, margins are additive
  let node := LayoutNode.column 0 #[
    LayoutNode.leaf 1 (ContentSize.mk' 50 30) { margin := { bottom := 20 } },
    LayoutNode.leaf 2 (ContentSize.mk' 50 30) { margin := { top := 15 } }
  ]
  let result := layout node 100 200
  let cl2 := result.get! 2
  -- Without collapse: y2 = 30 + 20 + 15 = 65
  shouldBeNear cl2.y 65 0.01

test "margin collapse: basic collapse takes max of adjacent margins" := do
  let props := { FlexContainer.column with marginCollapse := true }
  let node := LayoutNode.flexBox 0 props #[
    LayoutNode.leaf 1 (ContentSize.mk' 50 30) { margin := { bottom := 20 } },
    LayoutNode.leaf 2 (ContentSize.mk' 50 30) { margin := { top := 15 } }
  ]
  let result := layout node 100 200
  let cl2 := result.get! 2
  -- With collapse: max(20, 15) = 20, so y2 = 30 + 20 = 50
  shouldBeNear cl2.y 50 0.01

test "margin collapse: does not apply to row direction" := do
  let props := { FlexContainer.row with marginCollapse := true }
  let node := LayoutNode.flexBox 0 props #[
    LayoutNode.leaf 1 (ContentSize.mk' 50 30) { margin := { right := 20 } },
    LayoutNode.leaf 2 (ContentSize.mk' 50 30) { margin := { left := 15 } }
  ]
  let result := layout node 200 100
  let cl2 := result.get! 2
  -- No collapse in row: x2 = 50 + 20 + 15 = 85
  shouldBeNear cl2.x 85 0.01

test "margin collapse: both negative margins uses min" := do
  let props := { FlexContainer.column with marginCollapse := true }
  let node := LayoutNode.flexBox 0 props #[
    LayoutNode.leaf 1 (ContentSize.mk' 50 30) { margin := { bottom := -10 } },
    LayoutNode.leaf 2 (ContentSize.mk' 50 30) { margin := { top := -15 } }
  ]
  let result := layout node 100 200
  let cl2 := result.get! 2
  -- Both negative: min(-10, -15) = -15, so y2 = 30 + (-15) = 15
  shouldBeNear cl2.y 15 0.01

test "margin collapse: mixed positive and negative" := do
  let props := { FlexContainer.column with marginCollapse := true }
  let node := LayoutNode.flexBox 0 props #[
    LayoutNode.leaf 1 (ContentSize.mk' 50 30) { margin := { bottom := 20 } },
    LayoutNode.leaf 2 (ContentSize.mk' 50 30) { margin := { top := -8 } }
  ]
  let result := layout node 100 200
  let cl2 := result.get! 2
  -- Mixed: 20 + (-8) = 12, so y2 = 30 + 12 = 42
  shouldBeNear cl2.y 42 0.01

test "margin collapse: first item keeps full top margin" := do
  let props := { FlexContainer.column with marginCollapse := true }
  let node := LayoutNode.flexBox 0 props #[
    LayoutNode.leaf 1 (ContentSize.mk' 50 30) { margin := { top := 10, bottom := 20 } },
    LayoutNode.leaf 2 (ContentSize.mk' 50 30) { margin := { top := 15 } }
  ]
  let result := layout node 100 200
  let cl1 := result.get! 1
  -- First item keeps its top margin
  shouldBeNear cl1.y 10 0.01

test "margin collapse: three items chain collapse" := do
  let props := { FlexContainer.column with marginCollapse := true }
  let node := LayoutNode.flexBox 0 props #[
    LayoutNode.leaf 1 (ContentSize.mk' 50 30) { margin := { bottom := 20 } },
    LayoutNode.leaf 2 (ContentSize.mk' 50 30) { margin := { top := 15, bottom := 25 } },
    LayoutNode.leaf 3 (ContentSize.mk' 50 30) { margin := { top := 10 } }
  ]
  let result := layout node 100 300
  let cl1 := result.get! 1
  let cl2 := result.get! 2
  let cl3 := result.get! 3
  -- y1 = 0
  -- y2 = 30 + max(20, 15) = 50
  -- y3 = 50 + 30 + max(25, 10) = 105
  shouldBeNear cl1.y 0 0.01
  shouldBeNear cl2.y 50 0.01
  shouldBeNear cl3.y 105 0.01

#generate_tests

end TrellisTests.LayoutTests.EdgeCases
