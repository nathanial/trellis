/-
  Trellis Layout Tests - Grid
  Unit tests for the CSS Grid layout system, including auto-placement,
  track sizing functions (fr, minmax, repeat), and alignment.
-/
import Crucible
import Trellis

namespace TrellisTests.LayoutTests.Grid

open Crucible
open Trellis

testSuite "Trellis Layout Tests - Grid"

/-! ## Grid Basic Tests -/

test "grid with 3 equal fr columns" := do
  let props := GridContainer.columns 3
  let node := LayoutNode.gridBox 0 props #[
    LayoutNode.leaf 1 (ContentSize.mk' 0 30),
    LayoutNode.leaf 2 (ContentSize.mk' 0 30),
    LayoutNode.leaf 3 (ContentSize.mk' 0 30)
  ]
  let result := layout node 300 100
  let cl1 := result.get! 1
  let cl2 := result.get! 2
  let cl3 := result.get! 3
  -- Each column should be 100px wide (300 / 3)
  shouldBeNear cl1.width 100 0.01
  shouldBeNear cl2.width 100 0.01
  shouldBeNear cl3.width 100 0.01
  -- Items placed left to right
  shouldBeNear cl1.x 0 0.01
  shouldBeNear cl2.x 100 0.01
  shouldBeNear cl3.x 200 0.01

test "grid with mixed track sizes (fixed + fr)" := do
  let props := GridContainer.withColumns #[.px 50, .fr 1, .fr 2]
  let node := LayoutNode.gridBox 0 props #[
    LayoutNode.leaf 1 (ContentSize.mk' 0 30),
    LayoutNode.leaf 2 (ContentSize.mk' 0 30),
    LayoutNode.leaf 3 (ContentSize.mk' 0 30)
  ]
  let result := layout node 350 100
  let cl1 := result.get! 1
  let cl2 := result.get! 2
  let cl3 := result.get! 3
  -- First column: fixed 50px
  -- Remaining: 300px, split 1:2 = 100px and 200px
  shouldBeNear cl1.width 50 0.01
  shouldBeNear cl2.width 100 0.01
  shouldBeNear cl3.width 200 0.01

test "grid respects column and row gaps" := do
  let props := GridContainer.columns 2 (gap := 20)
  let node := LayoutNode.gridBox 0 props #[
    LayoutNode.leaf 1 (ContentSize.mk' 0 30),
    LayoutNode.leaf 2 (ContentSize.mk' 0 30),
    LayoutNode.leaf 3 (ContentSize.mk' 0 30),
    LayoutNode.leaf 4 (ContentSize.mk' 0 30)
  ]
  let result := layout node 220 100
  let cl1 := result.get! 1
  let cl2 := result.get! 2
  let cl3 := result.get! 3
  -- 220 - 20 gap = 200, split into 2 columns = 100 each
  shouldBeNear cl1.width 100 0.01
  shouldBeNear cl2.width 100 0.01
  -- Second column starts after first + gap
  shouldBeNear cl2.x 120 0.01
  -- Third item is on second row
  shouldSatisfy (cl3.y > cl1.y) "row 2 should be below row 1"

/-! ## Grid Auto-Placement Tests -/

test "grid auto-places items in row-major order" := do
  let props := GridContainer.columns 3
  let node := LayoutNode.gridBox 0 props #[
    LayoutNode.leaf 1 (ContentSize.mk' 0 30),
    LayoutNode.leaf 2 (ContentSize.mk' 0 30),
    LayoutNode.leaf 3 (ContentSize.mk' 0 30),
    LayoutNode.leaf 4 (ContentSize.mk' 0 30),
    LayoutNode.leaf 5 (ContentSize.mk' 0 30)
  ]
  let result := layout node 300 200
  let cl1 := result.get! 1
  let cl2 := result.get! 2
  let cl3 := result.get! 3
  let cl4 := result.get! 4
  let cl5 := result.get! 5
  -- First row: items 1, 2, 3
  shouldBeNear cl1.y cl2.y 0.01
  shouldBeNear cl2.y cl3.y 0.01
  -- Second row: items 4, 5
  shouldSatisfy (cl4.y > cl1.y) "item 4 should be on row 2"
  shouldBeNear cl4.y cl5.y 0.01
  -- Item 4 should be at column 0
  shouldBeNear cl4.x 0 0.01

/-! ## Grid Auto-Flow Tests -/

test "grid-auto-flow: row places items left-to-right, top-to-bottom" := do
  let props := { GridContainer.columns 3 with autoFlow := .row }
  let node := LayoutNode.gridBox 0 props #[
    LayoutNode.leaf 1 (ContentSize.mk' 0 30),
    LayoutNode.leaf 2 (ContentSize.mk' 0 30),
    LayoutNode.leaf 3 (ContentSize.mk' 0 30),
    LayoutNode.leaf 4 (ContentSize.mk' 0 30)
  ]
  let result := layout node 300 200
  let cl1 := result.get! 1
  let cl2 := result.get! 2
  let cl3 := result.get! 3
  let cl4 := result.get! 4
  -- Items 1, 2, 3 on row 1 (same y)
  shouldBeNear cl1.y cl2.y 0.01
  shouldBeNear cl2.y cl3.y 0.01
  -- Item 4 on row 2, column 1
  shouldSatisfy (cl4.y > cl1.y) "item 4 should be on row 2"
  shouldBeNear cl4.x 0 0.01

test "grid-auto-flow: column places items top-to-bottom, left-to-right" := do
  let props := { GridContainer.withTemplate #[.fr 1, .fr 1, .fr 1] #[.fr 1, .fr 1] with
    autoFlow := .column
  }
  let node := LayoutNode.gridBox 0 props #[
    LayoutNode.leaf 1 (ContentSize.mk' 0 30),
    LayoutNode.leaf 2 (ContentSize.mk' 0 30),
    LayoutNode.leaf 3 (ContentSize.mk' 0 30),
    LayoutNode.leaf 4 (ContentSize.mk' 0 30)
  ]
  let result := layout node 200 300
  let cl1 := result.get! 1
  let cl2 := result.get! 2
  let cl3 := result.get! 3
  let cl4 := result.get! 4
  -- Items 1, 2, 3 in column 1 (same x)
  shouldBeNear cl1.x cl2.x 0.01
  shouldBeNear cl2.x cl3.x 0.01
  -- Item 4 in column 2
  shouldSatisfy (cl4.x > cl1.x) "item 4 should be in column 2"
  shouldBeNear cl4.y 0 0.01

test "grid-auto-flow: rowDense backfills gaps" := do
  let props := { GridContainer.columns 3 with autoFlow := .rowDense }
  -- Item 1 spans 2 columns starting at col 0, leaving col 2 open
  -- Item 2 is 1x1, with dense it should fill (0, 2)
  let spanItem := { GridItem.default with
    placement := { column := GridSpan.spanTracks 2 }
  }
  let node := LayoutNode.gridBox 0 props #[
    LayoutNode.leaf' 1 0 30 {} (.gridChild spanItem),
    LayoutNode.leaf 2 (ContentSize.mk' 0 30),
    LayoutNode.leaf 3 (ContentSize.mk' 0 30)
  ]
  let result := layout node 300 200
  let cl1 := result.get! 1
  let cl2 := result.get! 2
  let cl3 := result.get! 3
  -- Item 1 at (0, 0), spans columns 0-1
  shouldBeNear cl1.x 0 0.01
  shouldBeNear cl1.width 200 0.01
  -- Item 2 fills the gap at column 2, same row as item 1
  shouldBeNear cl2.x 200 0.01
  shouldBeNear cl2.y cl1.y 0.01
  -- Item 3 on row 2
  shouldSatisfy (cl3.y > cl1.y) "item 3 should be on row 2"

test "grid-auto-flow: row sparse leaves gaps" := do
  let props := { GridContainer.columns 3 with autoFlow := .row }
  -- Item 1 is 1x1 at (0,0)
  -- Item 2 is 2-column span, can't fit in remaining 2 cells if starting from cursor
  -- Item 3 with sparse mode should NOT backfill gap at (0,1)
  let spanItem := { GridItem.default with
    placement := { column := GridSpan.spanTracks 2 }
  }
  let node := LayoutNode.gridBox 0 props #[
    LayoutNode.leaf 1 (ContentSize.mk' 0 30),
    LayoutNode.leaf' 2 0 30 {} (.gridChild spanItem),
    LayoutNode.leaf 3 (ContentSize.mk' 0 30)
  ]
  let result := layout node 300 200
  let cl1 := result.get! 1
  let cl2 := result.get! 2
  let cl3 := result.get! 3
  -- Item 1 at (0, 0)
  shouldBeNear cl1.x 0 0.01
  shouldBeNear cl1.y 0 0.01
  -- Item 2 at (0, 1) since it fits in remaining 2 columns
  shouldBeNear cl2.x 100 0.01
  shouldBeNear cl2.y cl1.y 0.01
  -- Item 3 continues from where item 2 ended
  shouldSatisfy (cl3.y > cl1.y) "item 3 should be on row 2"
  shouldBeNear cl3.x 0 0.01

test "grid-auto-flow: columnDense backfills column gaps" := do
  let props := { GridContainer.withTemplate #[.fr 1, .fr 1, .fr 1] #[.fr 1, .fr 1] with
    autoFlow := .columnDense
  }
  let spanItem := { GridItem.default with
    placement := { row := GridSpan.spanTracks 2 }
  }
  let node := LayoutNode.gridBox 0 props #[
    LayoutNode.leaf' 1 0 30 {} (.gridChild spanItem),  -- spans 2 rows in col 0
    LayoutNode.leaf 2 (ContentSize.mk' 0 30),
    LayoutNode.leaf 3 (ContentSize.mk' 0 30)
  ]
  let result := layout node 200 300
  let cl1 := result.get! 1
  let cl2 := result.get! 2
  let cl3 := result.get! 3
  -- Item 1 at column 0, spans 2 rows (taller than single-row items)
  shouldBeNear cl1.x 0 0.01
  shouldSatisfy (cl1.height > cl2.height) "item 1 should be taller (spans 2 rows)"
  -- Item 2 in same column as item 1, but below it (dense backfills row 2)
  shouldBeNear cl2.x 0 0.01
  shouldSatisfy (cl2.y > cl1.y) "item 2 should be below item 1"
  -- Item 3 in column 1 (next column in column-first flow)
  shouldSatisfy (cl3.x > cl1.x) "item 3 should be in column 2"

test "grid-auto-flow: column creates implicit columns" := do
  let props := { GridContainer.withTemplate #[.fr 1, .fr 1] #[.fr 1] with
    autoFlow := .column
  }
  let node := LayoutNode.gridBox 0 props #[
    LayoutNode.leaf 1 (ContentSize.mk' 0 30),
    LayoutNode.leaf 2 (ContentSize.mk' 0 30),
    LayoutNode.leaf 3 (ContentSize.mk' 0 30)
  ]
  -- 2 explicit rows, 1 explicit column, 3 items
  -- Should create implicit columns
  let result := layout node 300 200
  let cl1 := result.get! 1
  let cl2 := result.get! 2
  let cl3 := result.get! 3
  -- Items 1, 2 fill column 1 (rows 0 and 1)
  shouldBeNear cl1.x cl2.x 0.01
  shouldSatisfy (cl2.y > cl1.y) "item 2 should be below item 1"
  -- Item 3 in implicit column 2
  shouldSatisfy (cl3.x > cl1.x) "item 3 should be in implicit column"

/-! ## Grid Explicit Placement Tests -/

test "grid places items at explicit positions" := do
  let props := GridContainer.withTemplate
    #[.fr 1, .fr 1]  -- 2 rows
    #[.fr 1, .fr 1, .fr 1]  -- 3 columns
  let node := LayoutNode.gridBox 0 props #[
    -- Place at row 2, col 3 (bottom-right)
    LayoutNode.leaf' 1 0 0 {} (.gridChild (GridItem.atPosition 2 3))
  ]
  let result := layout node 300 200
  let cl := result.get! 1
  -- Should be in bottom-right cell (col 2, row 1 in 0-indexed)
  shouldBeNear cl.x 200 0.01  -- column 3 starts at 200
  shouldBeNear cl.y 100 0.01  -- row 2 starts at 100

/-! ## Grid Named Lines and Areas Tests -/

test "grid named lines place items by line name" := do
  let columns : Array GridTrack := #[
    { size := .fr 1, name := some "left" },
    { size := .fr 1, name := some "main" },
    { size := .fr 1, name := some "right" }
  ]
  let props : GridContainer := {
    GridContainer.default with
      templateRows := GridTemplate.fromSizes #[.fr 1]
      templateColumns := { tracks := columns }
  }
  let itemProps := { GridItem.default with
    placement := {
      row := { start := .line 1 }
      column := { start := .named "main", finish := .named "right" }
    }
  }
  let node := LayoutNode.gridBox 0 props #[
    LayoutNode.leaf' 1 0 0 {} (.gridChild itemProps)
  ]
  let result := layout node 300 100
  let cl := result.get! 1
  shouldBeNear cl.x 100 0.01
  shouldBeNear cl.width 100 0.01

test "grid template areas place items by area name" := do
  let areas := GridTemplateAreas.fromStrings #[
    #["header", "header", "header"],
    #["sidebar", "content", "content"],
    #["footer", "footer", "footer"]
  ]
  let props : GridContainer := {
    GridContainer.default with
      templateRows := GridTemplate.fromSizes #[.fr 1, .fr 1, .fr 1]
      templateColumns := GridTemplate.fromSizes #[.fr 1, .fr 1, .fr 1]
      templateAreas := areas
  }
  let node := LayoutNode.gridBox 0 props #[
    LayoutNode.leaf' 1 0 0 {} (.gridChild (GridItem.inArea "header")),
    LayoutNode.leaf' 2 0 0 {} (.gridChild (GridItem.inArea "sidebar")),
    LayoutNode.leaf' 3 0 0 {} (.gridChild (GridItem.inArea "content"))
  ]
  let result := layout node 300 300
  let header := result.get! 1
  let sidebar := result.get! 2
  let content := result.get! 3
  shouldBeNear header.y 0 0.01
  shouldBeNear header.width 300 0.01
  shouldBeNear header.height 100 0.01
  shouldBeNear sidebar.x 0 0.01
  shouldBeNear sidebar.y 100 0.01
  shouldBeNear sidebar.width 100 0.01
  shouldBeNear content.x 100 0.01
  shouldBeNear content.y 100 0.01
  shouldBeNear content.width 200 0.01

/-! ## Grid Spanning Tests -/

test "grid item spanning multiple columns" := do
  let props := GridContainer.columns 3
  let spanItem := { GridItem.default with
    placement := { column := GridSpan.spanTracks 2 }
  }
  let node := LayoutNode.gridBox 0 props #[
    LayoutNode.leaf' 1 0 30 {} (.gridChild spanItem),  -- spans 2 cols
    LayoutNode.leaf 2 (ContentSize.mk' 0 30)
  ]
  let result := layout node 300 100
  let cl1 := result.get! 1
  let cl2 := result.get! 2
  -- First item spans 2 columns = 200px
  shouldBeNear cl1.width 200 0.01
  -- Second item is in column 3
  shouldBeNear cl2.x 200 0.01
  shouldBeNear cl2.width 100 0.01

test "grid item spanning multiple rows" := do
  let props := GridContainer.columns 2
  let spanItem := { GridItem.default with
    placement := { row := GridSpan.spanTracks 2 }
  }
  let node := LayoutNode.gridBox 0 props #[
    LayoutNode.leaf' 1 0 0 {} (.gridChild spanItem),  -- spans 2 rows
    LayoutNode.leaf 2 (ContentSize.mk' 0 50),
    LayoutNode.leaf 3 (ContentSize.mk' 0 50)
  ]
  let result := layout node 200 200
  let cl1 := result.get! 1
  let cl2 := result.get! 2
  let cl3 := result.get! 3
  -- First item spans 2 rows
  shouldBeNear cl1.height 100 0.01
  -- Items 2 and 3 are in column 2, stacked vertically
  shouldBeNear cl2.x 100 0.01
  shouldBeNear cl3.x 100 0.01
  shouldSatisfy (cl3.y > cl2.y) "item 3 should be below item 2"

/-! ## Grid Alignment Tests -/

test "grid justify-items: center centers items horizontally in cells" := do
  let props := { GridContainer.columns 1 with justifyItems := .center }
  let node := LayoutNode.gridBox 0 props #[
    LayoutNode.leaf 1 (ContentSize.mk' 50 30)
  ]
  let result := layout node 200 100
  let cl := result.get! 1
  -- Item should be centered: (200 - 50) / 2 = 75
  shouldBeNear cl.x 75 0.01
  shouldBeNear cl.width 50 0.01

test "grid align-items: center centers items vertically in cells" := do
  -- Need explicit row template with fr to make row fill available space
  let props := { GridContainer.withTemplate #[.fr 1] #[.fr 1] with alignItems := .center }
  let node := LayoutNode.gridBox 0 props #[
    LayoutNode.leaf 1 (ContentSize.mk' 50 30)
  ]
  let result := layout node 200 100
  let cl := result.get! 1
  -- Item should be centered: (100 - 30) / 2 = 35
  shouldBeNear cl.y 35 0.01
  shouldBeNear cl.height 30 0.01

test "grid stretch makes items fill cells" := do
  -- Need explicit row template with fr to make row fill available space
  let props := { GridContainer.withTemplate #[.fr 1] #[.fr 1, .fr 1] with
    justifyItems := .stretch
    alignItems := .stretch
  }
  let node := LayoutNode.gridBox 0 props #[
    LayoutNode.leaf 1 (ContentSize.mk' 30 20)
  ]
  let result := layout node 200 100
  let cl := result.get! 1
  -- Item should stretch to fill cell
  shouldBeNear cl.width 100 0.01
  shouldBeNear cl.height 100 0.01

test "grid justify-self overrides container justify-items" := do
  let props := { GridContainer.columns 1 with justifyItems := .stretch }
  let itemProps := { GridItem.default with justifySelf := some .flexEnd }
  let node := LayoutNode.gridBox 0 props #[
    LayoutNode.leaf' 1 50 30 {} (.gridChild itemProps)
  ]
  let result := layout node 200 100
  let cl := result.get! 1
  -- Item should be at end despite container stretch
  shouldBeNear cl.x 150 0.01
  shouldBeNear cl.width 50 0.01

/-! ## Grid Edge Cases -/

test "empty grid container produces no child layouts" := do
  let props := GridContainer.columns 3
  let node := LayoutNode.gridBox 0 props #[]
  let result := layout node 300 100
  -- Only the container itself
  result.size ≡ 1

test "single item grid fills available space with fr rows" := do
  -- Need explicit row template with fr to make row fill available space
  let props := GridContainer.withTemplate #[.fr 1] #[.fr 1]
  let node := LayoutNode.gridBox 0 props #[
    LayoutNode.leaf 1 (ContentSize.mk' 50 30)
  ]
  let result := layout node 200 100
  let cl := result.get! 1
  -- With stretch (default) and fr row, should fill the cell
  shouldBeNear cl.width 200 0.01
  shouldBeNear cl.height 100 0.01

test "grid creates implicit rows for overflow items" := do
  -- Only 1 explicit row, but 4 items in 2 columns
  let props := GridContainer.withTemplate #[.fr 1] #[.fr 1, .fr 1]
  let node := LayoutNode.gridBox 0 props #[
    LayoutNode.leaf 1 (ContentSize.mk' 0 50),
    LayoutNode.leaf 2 (ContentSize.mk' 0 50),
    LayoutNode.leaf 3 (ContentSize.mk' 0 50),
    LayoutNode.leaf 4 (ContentSize.mk' 0 50)
  ]
  let result := layout node 200 200
  let cl3 := result.get! 3
  let cl4 := result.get! 4
  -- Items 3 and 4 should be on implicit row 2
  shouldSatisfy (cl3.y > 0) "item 3 should be on row 2"
  shouldBeNear cl3.y cl4.y 0.01

/-! ## Grid minmax() Track Sizing Tests -/

test "grid minmax(px, px) clamps column width between min and max" := do
  -- minmax(50, 150) should clamp content-based sizing
  let props := GridContainer.withColumns #[.minmax (.px 50) (.px 150), .fr 1]
  let node := LayoutNode.gridBox 0 props #[
    LayoutNode.leaf 1 (ContentSize.mk' 200 30),  -- Content wants 200px but max is 150
    LayoutNode.leaf 2 (ContentSize.mk' 0 30)
  ]
  let result := layout node 300 100
  let cl1 := result.get! 1
  let cl2 := result.get! 2
  -- First column should be clamped to 150 (max), second gets remaining 150
  shouldBeNear cl1.width 150 0.01
  shouldBeNear cl2.width 150 0.01

test "grid minmax(px, px) respects minimum when content is smaller" := do
  -- minmax(100, 200) with small content should use min of 100
  let props := GridContainer.withColumns #[.minmax (.px 100) (.px 200), .fr 1]
  let node := LayoutNode.gridBox 0 props #[
    LayoutNode.leaf 1 (ContentSize.mk' 20 30),  -- Content wants only 20px but min is 100
    LayoutNode.leaf 2 (ContentSize.mk' 0 30)
  ]
  let result := layout node 300 100
  let cl1 := result.get! 1
  let cl2 := result.get! 2
  -- First column should be at least 100 (min), second gets remaining 200
  shouldBeNear cl1.width 100 0.01
  shouldBeNear cl2.width 200 0.01

test "grid minmax(px, fr) uses min as base and grows with fr" := do
  -- minmax(50, 1fr) should start at 50 and grow with remaining space
  let props := GridContainer.withColumns #[.minmax (.px 50) (.fr 1), .minmax (.px 50) (.fr 1)]
  let node := LayoutNode.gridBox 0 props #[
    LayoutNode.leaf 1 (ContentSize.mk' 0 30),
    LayoutNode.leaf 2 (ContentSize.mk' 0 30)
  ]
  let result := layout node 300 100
  let cl1 := result.get! 1
  let cl2 := result.get! 2
  -- Both columns should split remaining space equally: 300/2 = 150 each
  shouldBeNear cl1.width 150 0.01
  shouldBeNear cl2.width 150 0.01

test "grid minmax(px, fr) with unequal fr values" := do
  -- minmax(0, 1fr) vs minmax(0, 2fr) should distribute 1:2
  let props := GridContainer.withColumns #[.minmax (.px 0) (.fr 1), .minmax (.px 0) (.fr 2)]
  let node := LayoutNode.gridBox 0 props #[
    LayoutNode.leaf 1 (ContentSize.mk' 0 30),
    LayoutNode.leaf 2 (ContentSize.mk' 0 30)
  ]
  let result := layout node 300 100
  let cl1 := result.get! 1
  let cl2 := result.get! 2
  -- 1:2 ratio, so 100 and 200
  shouldBeNear cl1.width 100 0.01
  shouldBeNear cl2.width 200 0.01

test "grid minmax with row sizing" := do
  -- Test minmax on rows
  let props := GridContainer.withTemplate
    #[.minmax (.px 50) (.px 100)]  -- Row with minmax
    #[.fr 1]                        -- Single column
  let node := LayoutNode.gridBox 0 props #[
    LayoutNode.leaf 1 (ContentSize.mk' 50 30)   -- Content height 30 < min 50
  ]
  let result := layout node 200 200
  let cl := result.get! 1
  -- Row should be at least 50 (min)
  shouldSatisfy (cl.height >= 50) "row height should be at least minmax min"

test "grid mixed fixed and minmax columns" := do
  -- Fixed 50px + minmax(50, 100) + fr
  let props := GridContainer.withColumns #[.px 50, .minmax (.px 50) (.px 100), .fr 1]
  let node := LayoutNode.gridBox 0 props #[
    LayoutNode.leaf 1 (ContentSize.mk' 0 30),
    LayoutNode.leaf 2 (ContentSize.mk' 80 30),  -- Content 80 within minmax range
    LayoutNode.leaf 3 (ContentSize.mk' 0 30)
  ]
  let result := layout node 300 100
  let cl1 := result.get! 1
  let cl2 := result.get! 2
  let cl3 := result.get! 3
  -- First: fixed 50, Second: content 80 clamped to [50,100], Third: remaining
  shouldBeNear cl1.width 50 0.01
  shouldBeNear cl2.width 80 0.01
  shouldBeNear cl3.width 170 0.01  -- 300 - 50 - 80

/-! ## Grid repeat() Function Tests -/

test "grid repeat(3, 1fr) creates 3 equal columns" := do
  let props := { GridContainer.default with
    templateColumns := GridTemplate.repeatCount 3 #[.fr 1]
  }
  let node := LayoutNode.gridBox 0 props #[
    LayoutNode.leaf 1 (ContentSize.mk' 0 30),
    LayoutNode.leaf 2 (ContentSize.mk' 0 30),
    LayoutNode.leaf 3 (ContentSize.mk' 0 30)
  ]
  let result := layout node 300 100
  let cl1 := result.get! 1
  let cl2 := result.get! 2
  let cl3 := result.get! 3
  -- Each column should be 100px (300 / 3)
  shouldBeNear cl1.width 100 0.01
  shouldBeNear cl2.width 100 0.01
  shouldBeNear cl3.width 100 0.01

test "grid repeat(2, 50px 100px) creates alternating columns" := do
  let props := { GridContainer.default with
    templateColumns := GridTemplate.repeatCount 2 #[.px 50, .px 100]
  }
  let node := LayoutNode.gridBox 0 props #[
    LayoutNode.leaf 1 (ContentSize.mk' 0 30),
    LayoutNode.leaf 2 (ContentSize.mk' 0 30),
    LayoutNode.leaf 3 (ContentSize.mk' 0 30),
    LayoutNode.leaf 4 (ContentSize.mk' 0 30)
  ]
  let result := layout node 400 100
  let cl1 := result.get! 1
  let cl2 := result.get! 2
  let cl3 := result.get! 3
  let cl4 := result.get! 4
  -- Pattern: 50, 100, 50, 100
  shouldBeNear cl1.width 50 0.01
  shouldBeNear cl2.width 100 0.01
  shouldBeNear cl3.width 50 0.01
  shouldBeNear cl4.width 100 0.01

test "grid repeat(auto-fill, 100px) fills container with columns" := do
  let props := { GridContainer.default with
    templateColumns := GridTemplate.autoFill #[.px 100]
  }
  let node := LayoutNode.gridBox 0 props #[
    LayoutNode.leaf 1 (ContentSize.mk' 0 30),
    LayoutNode.leaf 2 (ContentSize.mk' 0 30),
    LayoutNode.leaf 3 (ContentSize.mk' 0 30)
  ]
  -- 350px container should fit 3 columns of 100px each
  let result := layout node 350 100
  let cl1 := result.get! 1
  let cl2 := result.get! 2
  let cl3 := result.get! 3
  -- All items should be on the first row (same y)
  shouldBeNear cl1.y cl2.y 0.01
  shouldBeNear cl2.y cl3.y 0.01
  -- Each column 100px wide
  shouldBeNear cl1.width 100 0.01

test "grid repeat(auto-fill, minmax(100px, 1fr)) creates responsive columns" := do
  let props := { GridContainer.default with
    templateColumns := GridTemplate.autoFill #[.minmax (.px 100) (.fr 1)]
  }
  let node := LayoutNode.gridBox 0 props #[
    LayoutNode.leaf 1 (ContentSize.mk' 0 30),
    LayoutNode.leaf 2 (ContentSize.mk' 0 30)
  ]
  -- 250px container: 2 columns of 100px min each, remaining space distributed
  let result := layout node 250 100
  let cl1 := result.get! 1
  let cl2 := result.get! 2
  -- Both on same row
  shouldBeNear cl1.y cl2.y 0.01
  -- Each column should be 125px (250 / 2)
  shouldBeNear cl1.width 125 0.01
  shouldBeNear cl2.width 125 0.01

test "grid repeat with gap accounts for gaps in auto-fill" := do
  let props := { GridContainer.default with
    templateColumns := GridTemplate.autoFill #[.px 100]
    columnGap := 20
  }
  let node := LayoutNode.gridBox 0 props #[
    LayoutNode.leaf 1 (ContentSize.mk' 0 30),
    LayoutNode.leaf 2 (ContentSize.mk' 0 30)
  ]
  -- 220px container with 20px gap: 2 columns of 100px each (100 + 20 + 100 = 220)
  let result := layout node 220 100
  let cl1 := result.get! 1
  let cl2 := result.get! 2
  shouldBeNear cl1.width 100 0.01
  shouldBeNear cl2.width 100 0.01
  -- Second column starts at 120 (100 + 20 gap)
  shouldBeNear cl2.x 120 0.01

test "grid repeat(auto-fit, 100px) behaves like auto-fill" := do
  -- Note: Full auto-fit behavior (collapsing empty tracks) is complex
  -- For now, auto-fit creates the same number of tracks as auto-fill
  let props := { GridContainer.default with
    templateColumns := GridTemplate.autoFit #[.px 100]
  }
  let node := LayoutNode.gridBox 0 props #[
    LayoutNode.leaf 1 (ContentSize.mk' 0 30),
    LayoutNode.leaf 2 (ContentSize.mk' 0 30)
  ]
  let result := layout node 300 100
  let cl1 := result.get! 1
  let cl2 := result.get! 2
  -- Both items should have 100px width
  shouldBeNear cl1.width 100 0.01
  shouldBeNear cl2.width 100 0.01

test "grid repeat on rows creates multiple rows" := do
  let props := { GridContainer.default with
    templateColumns := GridTemplate.fromSizes #[.fr 1]
    templateRows := GridTemplate.repeatCount 3 #[.px 50]
  }
  let node := LayoutNode.gridBox 0 props #[
    LayoutNode.leaf 1 (ContentSize.mk' 0 0),
    LayoutNode.leaf 2 (ContentSize.mk' 0 0),
    LayoutNode.leaf 3 (ContentSize.mk' 0 0)
  ]
  let result := layout node 200 200
  let cl1 := result.get! 1
  let cl2 := result.get! 2
  let cl3 := result.get! 3
  -- Each row should be 50px tall
  shouldBeNear cl1.height 50 0.01
  shouldBeNear cl2.height 50 0.01
  shouldBeNear cl3.height 50 0.01
  -- Items stacked vertically
  shouldBeNear cl1.y 0 0.01
  shouldBeNear cl2.y 50 0.01
  shouldBeNear cl3.y 100 0.01

/-! ## Baseline Alignment Tests -/

test "grid baseline: items in same row align baselines" := do
  let props := { GridContainer.columns 2 with alignItems := .baseline }
  let node := LayoutNode.gridBox 0 props #[
    LayoutNode.leaf 1 (ContentSize.withBaseline 50 40 20),
    LayoutNode.leaf 2 (ContentSize.withBaseline 50 60 40)
  ]
  let result := layout node 200 100
  let cl1 := result.get! 1
  let cl2 := result.get! 2
  -- Both items in same row, baselines should align
  let baseline1 := cl1.y + 20
  let baseline2 := cl2.y + 40
  shouldBeNear baseline1 baseline2 0.01

test "grid baseline: different rows have independent baselines" := do
  -- 1-column grid forces items into different rows
  let props := { GridContainer.columns 1 with alignItems := .baseline }
  let node := LayoutNode.gridBox 0 props #[
    LayoutNode.leaf 1 (ContentSize.withBaseline 100 40 20),
    LayoutNode.leaf 2 (ContentSize.withBaseline 100 60 40)
  ]
  let result := layout node 200 200
  let cl1 := result.get! 1
  let cl2 := result.get! 2
  -- Items are in different rows, so baseline alignment is independent
  -- Each item should be at the top of its row (only item in each row)
  shouldBeNear cl1.y 0 0.01
  shouldSatisfy (cl2.y > cl1.y) "item 2 should be in second row"



end TrellisTests.LayoutTests.Grid
