/-
  Trellis Layout Tests - Basic
  Unit tests for the basic layout functionality, nested layouts, and dimensions.
-/
import Crucible
import Trellis

namespace TrellisTests.LayoutTests.Basic

open Crucible
open Trellis

testSuite "Trellis Layout Tests - Basic"

/-! ## Basic Layout Tests -/

test "single leaf node takes available space" := do
  let node := LayoutNode.leaf 1 (ContentSize.mk' 100 50)
  let result := layout node 400 300
  let cl := result.get! 1
  shouldBeNear cl.width 100 0.01
  shouldBeNear cl.height 50 0.01

test "leaf respects fixed width/height constraints" := do
  let node := LayoutNode.leaf 1 (ContentSize.mk' 100 50)
    (BoxConstraints.fixed 200 100)
  let result := layout node 400 300
  let cl := result.get! 1
  shouldBeNear cl.width 200 0.01
  shouldBeNear cl.height 100 0.01

/-! ## Nested Layout Tests -/

test "nested flex containers layout correctly" := do
  let inner := LayoutNode.column 10 #[
    LayoutNode.leaf 11 (ContentSize.mk' 40 20),
    LayoutNode.leaf 12 (ContentSize.mk' 40 20)
  ]
  let outer := LayoutNode.row 0 #[
    LayoutNode.leaf 1 (ContentSize.mk' 50 50),
    inner.withItem (.flexChild (FlexItem.growing 1))
  ]
  let result := layout outer 200 100
  let cl1 := result.get! 1
  let clInner := result.get! 10
  -- Outer leaf at start
  shouldBeNear cl1.x 0 0.01
  -- Inner container fills remaining space
  shouldBeNear clInner.x 50 0.01
  shouldBeNear clInner.width 150 0.01

test "row nested in column uses intrinsic height" := do
  let row := LayoutNode.row 2 #[
    LayoutNode.leaf 3 (ContentSize.mk' 4 2),
    LayoutNode.leaf 4 (ContentSize.mk' 5 3)
  ]
  let col := LayoutNode.column 0 #[
    LayoutNode.leaf 1 (ContentSize.mk' 10 1),
    row
  ]
  let result := layout col 20 5
  let clRow := result.get! 2
  let clChild1 := result.get! 3
  let clChild2 := result.get! 4
  shouldBeNear clRow.height 3 0.01
  shouldBeNear clChild1.height 3 0.01
  shouldBeNear clChild2.height 3 0.01

/-! ## Absolute Positioning Tests -/

test "absolute positioned child does not affect flex layout" := do
  let absBox : BoxConstraints := { position := .absolute, top := some 40, left := some 10 }
  let node := LayoutNode.row 0 #[
    LayoutNode.leaf 1 (ContentSize.mk' 50 20),
    LayoutNode.leaf 2 (ContentSize.mk' 80 30) absBox
  ]
  let result := layout node 200 100
  let cl1 := result.get! 1
  let cl2 := result.get! 2
  shouldBeNear cl1.x 0 0.01
  shouldBeNear cl1.width 50 0.01
  shouldBeNear cl2.x 10 0.01
  shouldBeNear cl2.y 40 0.01

/-! ## Percentage Dimension Tests -/

test "leaf with height: percent 100 fills available height" := do
  -- A leaf node with height: 100% should fill the available height
  let node := LayoutNode.leaf 1 (ContentSize.mk' 50 30) { height := .percent 1.0 }
  let result := layout node 200 300
  let cl := result.get! 1
  -- Should fill 100% of available height (300)
  shouldBeNear cl.height 300 0.01
  -- Width should still be content-based
  shouldBeNear cl.width 50 0.01

test "leaf with width: percent 100 fills available width" := do
  -- A leaf node with width: 100% should fill the available width
  let node := LayoutNode.leaf 1 (ContentSize.mk' 50 30) { width := .percent 1.0 }
  let result := layout node 200 300
  let cl := result.get! 1
  -- Should fill 100% of available width (200)
  shouldBeNear cl.width 200 0.01
  -- Height should still be content-based
  shouldBeNear cl.height 30 0.01

test "leaf with width and height percent 50 uses half of available" := do
  let node := LayoutNode.leaf 1 (ContentSize.mk' 0 0) { width := .percent 0.5, height := .percent 0.5 }
  let result := layout node 200 400
  let cl := result.get! 1
  shouldBeNear cl.width 100 0.01
  shouldBeNear cl.height 200 0.01

test "flex column child with height: percent 100 fills column" := do
  -- A child in a flex column with height: 100% should fill the column's height
  let node := LayoutNode.column 0 #[
    LayoutNode.leaf 1 (ContentSize.mk' 100 30) { height := .percent 1.0 }
  ]
  let result := layout node 200 300
  let cl := result.get! 1
  -- Child should fill the column's height (300)
  shouldBeNear cl.height 300 0.01

test "flex row child with width: percent 100 fills row" := do
  -- A child in a flex row with width: 100% should fill the row's width
  let node := LayoutNode.row 0 #[
    LayoutNode.leaf 1 (ContentSize.mk' 50 30) { width := .percent 1.0 }
  ]
  let result := layout node 200 300
  let cl := result.get! 1
  -- Child should fill the row's width (200)
  shouldBeNear cl.width 200 0.01

test "grid child with height: percent 100 fills cell height" := do
  -- A child in a grid cell with height: 100% should fill the cell's height
  let props := GridContainer.withTemplate #[.fr 1] #[.fr 1]  -- 1 row, 1 col with fr
  let node := LayoutNode.gridBox 0 props #[
    LayoutNode.leaf 1 (ContentSize.mk' 50 30) { height := .percent 1.0 }
  ]
  let result := layout node 200 300
  let cl := result.get! 1
  -- Child should fill the cell's height (300)
  shouldBeNear cl.height 300 0.01

test "grid child with width and height: percent 100 fills cell" := do
  -- A child in a grid cell with both dimensions at 100% should fill the cell
  let props := GridContainer.withTemplate #[.fr 1] #[.fr 1]
  let node := LayoutNode.gridBox 0 props #[
    LayoutNode.leaf 1 (ContentSize.mk' 0 0) { width := .percent 1.0, height := .percent 1.0 }
  ]
  let result := layout node 200 300
  let cl := result.get! 1
  shouldBeNear cl.width 200 0.01
  shouldBeNear cl.height 300 0.01

test "nested flex column child with height: percent 100" := do
  -- Inner column has height: 100%, should fill available cross axis space
  -- Note: In CSS Flexbox, percentage heights in flex children are relative to available space
  -- at measurement time, not the final computed size of siblings
  let inner := LayoutNode.column 10 #[
    LayoutNode.leaf 11 (ContentSize.mk' 50 20)
  ] (box := { height := .percent 1.0 })
  let outer := LayoutNode.row 0 #[
    LayoutNode.leaf 1 (ContentSize.mk' 50 100),
    inner
  ]
  let result := layout outer 200 150
  let clInner := result.get! 10
  -- Inner column fills 100% of available cross space (150)
  shouldBeNear clInner.height 150 0.01

test "grid with fr rows - items stretch to fill cells by default" := do
  -- Grid with 2 rows of 1fr each in 300px container = 150px per row
  -- Default alignItems is stretch, so items fill their cells
  let props := GridContainer.withTemplate #[.fr 1, .fr 1] #[.fr 1]
  let node := LayoutNode.gridBox 0 props #[
    LayoutNode.leaf 1 (ContentSize.mk' 50 30) { height := .percent 1.0 },
    LayoutNode.leaf 2 (ContentSize.mk' 50 30)
  ]
  let result := layout node 200 300
  let cl1 := result.get! 1
  let cl2 := result.get! 2
  -- First child has explicit height: 100%, fills cell
  shouldBeNear cl1.height 150 0.01
  -- Second child stretches to fill cell (default grid behavior)
  shouldBeNear cl2.height 150 0.01

test "grid with alignItems: flexStart - items use content size" := do
  -- Grid with alignItems: flexStart - items don't stretch
  let props := { GridContainer.withTemplate #[.fr 1, .fr 1] #[.fr 1] with alignItems := .flexStart }
  let node := LayoutNode.gridBox 0 props #[
    LayoutNode.leaf 1 (ContentSize.mk' 50 30) { height := .percent 1.0 },
    LayoutNode.leaf 2 (ContentSize.mk' 50 30)
  ]
  let result := layout node 200 300
  let cl1 := result.get! 1
  let cl2 := result.get! 2
  -- First child has explicit height: 100%, fills cell
  shouldBeNear cl1.height 150 0.01
  -- Second child uses content height (no stretch)
  shouldBeNear cl2.height 30 0.01

test "percent dimensions respect min constraints" := do
  -- percent 100 of 200 = 200, but minWidth is 250, so should be 250
  let node := LayoutNode.leaf 1 (ContentSize.mk' 50 30)
    { width := .percent 1.0, minWidth := 250 }
  let result := layout node 200 300
  let cl := result.get! 1
  shouldBeNear cl.width 250 0.01

test "percent dimensions respect max constraints" := do
  -- percent 100 of 200 = 200, but maxWidth is 150, so should be 150
  let node := LayoutNode.leaf 1 (ContentSize.mk' 50 30)
    { width := .percent 1.0, maxWidth := some 150 }
  let result := layout node 200 300
  let cl := result.get! 1
  shouldBeNear cl.width 150 0.01

/-! ## Nested Layout Height Tests -/

-- NOTE: In CSS Flexbox, height: 100% resolves to the container's height,
-- NOT "remaining space after siblings". To fill remaining space, use flex-grow.

test "flex column: flex-grow child fills remaining after fixed sibling" := do
  -- Column with fixed child + growing child
  -- Available: 300px, fixed: 50px, growing child gets remaining 250px
  let node := LayoutNode.column 0 #[
    LayoutNode.leaf 1 (ContentSize.mk' 100 50) {},  -- fixed 50px height
    LayoutNode.leaf 2 (ContentSize.mk' 100 30) {} (item := .flexChild (FlexItem.growing 1))
  ]
  let result := layout node 200 300
  let fixed := result.get! 1
  let growing := result.get! 2
  shouldBeNear fixed.height 50 0.01
  shouldBeNear growing.height 250 0.01  -- fills remaining via flex-grow

test "flex column: label + growing grid child fills remaining height" := do
  -- Simulates cellWidget: label (30px) + inner grid (flex-grow: 1)
  let gridProps := GridContainer.withTemplate #[.fr 1] #[.fr 1] 0
  let node := LayoutNode.column 0 #[
    LayoutNode.leaf 1 (ContentSize.mk' 100 30) {},  -- label
    LayoutNode.gridBox 2 gridProps #[
      LayoutNode.leaf 3 (ContentSize.mk' 50 50) { height := .percent 1.0 }
    ] (item := .flexChild (FlexItem.growing 1))
  ]
  let result := layout node 200 300
  let label := result.get! 1
  let grid := result.get! 2
  let gridChild := result.get! 3
  shouldBeNear label.height 30 0.01
  shouldBeNear grid.height 270 0.01      -- 300 - 30 via flex-grow
  shouldBeNear gridChild.height 270 0.01 -- fills grid cell via height: 100%

test "grid inside grid: inner grid fills outer cell" := do
  -- Outer 1x1 grid -> inner 2x2 grid with height: 100%
  let outerProps := GridContainer.withTemplate #[.fr 1] #[.fr 1] 0
  let innerProps := GridContainer.withTemplate #[.fr 1, .fr 1] #[.fr 1, .fr 1] 0
  let node := LayoutNode.gridBox 0 outerProps #[
    LayoutNode.gridBox 1 innerProps #[
      LayoutNode.leaf 2 (ContentSize.mk' 30 30) { height := .percent 1.0 },
      LayoutNode.leaf 3 (ContentSize.mk' 30 30) { height := .percent 1.0 },
      LayoutNode.leaf 4 (ContentSize.mk' 30 30) { height := .percent 1.0 },
      LayoutNode.leaf 5 (ContentSize.mk' 30 30) { height := .percent 1.0 }
    ] (box := { height := .percent 1.0 })
  ]
  let result := layout node 200 300
  let innerGrid := result.get! 1
  shouldBeNear innerGrid.height 300 0.01
  -- Each inner cell: 150px high (300/2)
  for id in [2, 3, 4, 5] do
    let cell := result.get! id
    shouldBeNear cell.height 150 0.01

test "deeply nested: column > grid > column with flex-grow" := do
  -- Simulates: cellWidget column > gridFlex > cardStyleFlex column
  -- Outer column: label + grid with flex-grow
  -- Grid: contains column that fills cell via height: 100%
  let gridProps := GridContainer.withTemplate #[.fr 1] #[.fr 1] 0
  let node := LayoutNode.column 0 #[
    LayoutNode.leaf 1 (ContentSize.mk' 100 20) {},  -- outer label
    LayoutNode.gridBox 2 gridProps #[
      LayoutNode.column 3 #[
        LayoutNode.leaf 4 (ContentSize.mk' 50 50) {},  -- card content
        LayoutNode.leaf 5 (ContentSize.mk' 50 15) {}   -- card label
      ] (box := { height := .percent 1.0 })
    ] (item := .flexChild (FlexItem.growing 1))  -- flex-grow to fill remaining
  ]
  let result := layout node 200 300
  let outerLabel := result.get! 1
  let grid := result.get! 2
  let cardColumn := result.get! 3
  shouldBeNear outerLabel.height 20 0.01
  shouldBeNear grid.height 280 0.01        -- 300 - 20 via flex-grow
  shouldBeNear cardColumn.height 280 0.01  -- fills grid cell via height: 100%

test "flex column child content expands when height: percent 1.0" := do
  -- First child has small content but height: 100%, should expand
  let node := LayoutNode.column 0 #[
    LayoutNode.leaf 1 (ContentSize.mk' 50 30) { height := .percent 1.0 }
  ]
  let result := layout node 200 300
  let child := result.get! 1
  shouldBeNear child.height 300 0.01  -- should fill, not stay at 30

/-! ## Aspect Ratio Tests -/

test "aspect-ratio: width set, height computed" := do
  let node := LayoutNode.leaf 1 (ContentSize.mk' 50 50)
    { width := .length 160, aspectRatio := some (16.0 / 9.0) }
  let result := layout node 400 300
  let cl := result.get! 1
  shouldBeNear cl.width 160 0.01
  shouldBeNear cl.height 90 0.01  -- 160 / (16/9) = 90

test "aspect-ratio: height set, width computed" := do
  let node := LayoutNode.leaf 1 (ContentSize.mk' 50 50)
    { height := .length 90, aspectRatio := some (16.0 / 9.0) }
  let result := layout node 400 300
  let cl := result.get! 1
  shouldBeNear cl.width 160 0.01  -- 90 * (16/9) = 160
  shouldBeNear cl.height 90 0.01

test "aspect-ratio: both dimensions set, ratio ignored" := do
  let node := LayoutNode.leaf 1 (ContentSize.mk' 50 50)
    { width := .length 100, height := .length 100, aspectRatio := some (16.0 / 9.0) }
  let result := layout node 400 300
  let cl := result.get! 1
  shouldBeNear cl.width 100 0.01
  shouldBeNear cl.height 100 0.01

test "aspect-ratio: respects max-height constraint" := do
  let node := LayoutNode.leaf 1 (ContentSize.mk' 50 50)
    { width := .length 160, maxHeight := some 50, aspectRatio := some (16.0 / 9.0) }
  let result := layout node 400 300
  let cl := result.get! 1
  shouldBeNear cl.width 160 0.01
  shouldBeNear cl.height 50 0.01  -- Clamped from 90 to 50

test "aspect-ratio: in flex row" := do
  let node := LayoutNode.row 0 #[
    LayoutNode.leaf 1 (ContentSize.mk' 50 50) { width := .length 80, aspectRatio := some 2.0 }
  ]
  let result := layout node 400 300
  let cl := result.get! 1
  shouldBeNear cl.width 80 0.01
  shouldBeNear cl.height 40 0.01  -- 80 / 2 = 40

test "aspect-ratio: 1:1 square" := do
  let node := LayoutNode.leaf 1 (ContentSize.mk' 50 50)
    { width := .length 100, aspectRatio := some 1.0 }
  let result := layout node 400 300
  let cl := result.get! 1
  shouldBeNear cl.width 100 0.01
  shouldBeNear cl.height 100 0.01



end TrellisTests.LayoutTests.Basic
