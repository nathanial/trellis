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

/-! ## Grid with Wrapped Flex Children -/

test "grid auto row sizes correctly for wrapped flex child" := do
  -- Grid with: auto row (tab bar), fixed 1px divider, fr row (content)
  -- The first row contains a wrapped flex container with 4 tabs
  -- In a 200px wide container, 4 x 60px tabs should wrap to 2 lines
  let gridProps := GridContainer.withTemplate
    #[.auto, .fixed (.length 1.0), .fr 1]  -- 3 rows: auto, 1px, fill
    #[.fr 1]                                -- 1 column
  let flexProps : FlexContainer := {
    direction := .row
    wrap := .wrap
    gap := 0
    rowGap := 0
  }
  -- Tab bar: 4 tabs, each 60px wide x 30px tall
  -- In 200px container: 3 tabs fit on first line, 1 wraps to second line
  -- Expected tab bar height: 60px (2 lines x 30px each)
  let tabBar := LayoutNode.flexBox 1 flexProps #[
    LayoutNode.leaf 2 (ContentSize.mk' 60 30),
    LayoutNode.leaf 3 (ContentSize.mk' 60 30),
    LayoutNode.leaf 4 (ContentSize.mk' 60 30),
    LayoutNode.leaf 5 (ContentSize.mk' 60 30)
  ]
  let divider := LayoutNode.leaf 6 (ContentSize.mk' 0 1)
  let content := LayoutNode.leaf 7 (ContentSize.mk' 0 50)

  let node := LayoutNode.gridBox 0 gridProps #[tabBar, divider, content]
  let result := layout node 200 200

  let tabBarLayout := result.get! 1
  let dividerLayout := result.get! 6
  let contentLayout := result.get! 7

  -- Tab bar should be 60px tall (2 wrapped lines of 30px each)
  shouldBeNear tabBarLayout.height 60 0.01

  -- Divider should start at y=60 (after the 2-line tab bar)
  shouldBeNear dividerLayout.y 60 0.01
  shouldBeNear dividerLayout.height 1 0.01

  -- Content should start at y=61 (after tab bar + divider)
  shouldBeNear contentLayout.y 61 0.01

  -- Content should NOT overlap with tab bar
  -- (This is the actual bug: content starts at ~31 instead of 61)
  shouldSatisfy (contentLayout.y >= tabBarLayout.y + tabBarLayout.height)
    "content should not overlap with tab bar"

test "grid auto row sizes correctly for wrapped flex child with padding" := do
  -- Same as above, but the tab bar has padding that should be included in its height
  let gridProps := GridContainer.withTemplate
    #[.auto, .fixed (.length 1.0), .fr 1]
    #[.fr 1]
  let flexProps : FlexContainer := {
    direction := .row
    wrap := .wrap
    gap := 0
    rowGap := 0
  }
  -- Tab bar: 4 tabs, each 60x30; wraps to 2 lines in 200px width
  -- Padding vertical = 12px (6 top + 6 bottom), so total height = 60 + 12 = 72
  let tabBar := LayoutNode.flexBox 1 flexProps #[
    LayoutNode.leaf 2 (ContentSize.mk' 60 30),
    LayoutNode.leaf 3 (ContentSize.mk' 60 30),
    LayoutNode.leaf 4 (ContentSize.mk' 60 30),
    LayoutNode.leaf 5 (ContentSize.mk' 60 30)
  ] (box := { padding := Trellis.EdgeInsets.symmetric 8 6 })
  let divider := LayoutNode.leaf 6 (ContentSize.mk' 0 1)
  let content := LayoutNode.leaf 7 (ContentSize.mk' 0 50)

  let node := LayoutNode.gridBox 0 gridProps #[tabBar, divider, content]
  let result := layout node 200 200

  let tabBarLayout := result.get! 1
  let dividerLayout := result.get! 6
  let contentLayout := result.get! 7

  shouldBeNear tabBarLayout.height 72 0.01
  shouldBeNear dividerLayout.y 72 0.01
  shouldBeNear contentLayout.y 73 0.01
  shouldSatisfy (contentLayout.y >= tabBarLayout.y + tabBarLayout.height)
    "content should not overlap with padded tab bar"

test "nested layout: column containing grid with wrapped flex and nested flex content" := do
  -- This mimics the ReactiveShowcase structure:
  -- Root: flex column
  --   - Title (leaf)
  --   - TabView-like grid (fills remaining space)
  --     - Row 0 (auto): wrapped flex tab bar
  --     - Row 1 (1px): divider
  --     - Row 2 (fr): content panel (flex column containing flex rows)

  -- Tab bar: wrapped horizontal flex with 8 tabs
  let tabBarProps : FlexContainer := {
    direction := .row
    wrap := .wrap
    gap := 4
    rowGap := 4
  }
  let tabBar := LayoutNode.flexBox 10 tabBarProps #[
    LayoutNode.leaf 11 (ContentSize.mk' 80 30),
    LayoutNode.leaf 12 (ContentSize.mk' 80 30),
    LayoutNode.leaf 13 (ContentSize.mk' 80 30),
    LayoutNode.leaf 14 (ContentSize.mk' 80 30),
    LayoutNode.leaf 15 (ContentSize.mk' 80 30),
    LayoutNode.leaf 16 (ContentSize.mk' 80 30),
    LayoutNode.leaf 17 (ContentSize.mk' 80 30),
    LayoutNode.leaf 18 (ContentSize.mk' 80 30)
  ]

  -- Divider
  let divider := LayoutNode.leaf 20 (ContentSize.mk' 0 1)

  -- Content panel: flex column containing a flex row with two columns
  let leftCol := LayoutNode.column 31 #[
    LayoutNode.leaf 32 (ContentSize.mk' 100 40),
    LayoutNode.leaf 33 (ContentSize.mk' 100 40)
  ] (gap := 8)

  let rightCol := LayoutNode.column 41 #[
    LayoutNode.leaf 42 (ContentSize.mk' 100 40),
    LayoutNode.leaf 43 (ContentSize.mk' 100 40)
  ] (gap := 8)

  let contentRow := LayoutNode.row 30 #[leftCol, rightCol] (gap := 16)
  let contentPanel := LayoutNode.column 21 #[contentRow] (gap := 0)

  -- TabView-like grid
  let gridProps := GridContainer.withTemplate
    #[.auto, .fixed (.length 1.0), .fr 1]
    #[.fr 1]
  let tabViewGrid := LayoutNode.gridBox 2 gridProps #[tabBar, divider, contentPanel]

  -- Title
  let title := LayoutNode.leaf 1 (ContentSize.mk' 200 30)

  -- Root column
  let root := LayoutNode.column 0 #[title, tabViewGrid] (gap := 16)

  -- Layout in a 500x400 container
  -- 8 tabs of 80px + gaps: 8*80 + 7*4 = 668px > 500px, so tabs will wrap
  let result := layout root 500 400

  let titleLayout := result.get! 1
  let gridLayout := result.get! 2
  let tabBarLayout := result.get! 10
  let dividerLayout := result.get! 20
  let contentPanelLayout := result.get! 21
  let contentRowLayout := result.get! 30
  let leftColLayout := result.get! 31
  let rightColLayout := result.get! 41

  -- Title should be at top
  shouldBeNear titleLayout.y 0 0.01
  shouldBeNear titleLayout.height 30 0.01

  -- Grid should start after title + gap
  shouldBeNear gridLayout.y 46 0.01  -- 30 + 16

  -- Tab bar: 8 tabs of 80px + 7*4px gaps = 668px
  -- In 500px width: first line fits ~5 tabs (5*80 + 4*4 = 416), second line has 3 tabs
  -- Actually: 80+4+80+4+80+4+80+4+80 = 416, then 80+4+80+4+80 = 252
  -- So 5 tabs on first line, 3 on second = 2 lines of 30px each + 4px rowGap = 64px
  shouldBeNear tabBarLayout.height 64 0.01

  -- Divider should be after tab bar
  shouldBeNear dividerLayout.y (tabBarLayout.y + tabBarLayout.height) 0.01

  -- Content panel should be after divider
  shouldBeNear contentPanelLayout.y (dividerLayout.y + 1) 0.01

  -- Content panel should have positive height (not zero or negative)
  shouldSatisfy (contentPanelLayout.height > 50) "content panel should have reasonable height"

  -- Content row should render within content panel
  shouldSatisfy (contentRowLayout.y >= contentPanelLayout.y)
    "content row should be within content panel"

  -- Left and right columns should both be visible (non-zero width)
  shouldSatisfy (leftColLayout.width > 0) "left column should have width"
  shouldSatisfy (rightColLayout.width > 0) "right column should have width"

  -- Columns should be side by side
  shouldSatisfy (rightColLayout.x > leftColLayout.x) "right column should be to the right of left"

  -- Nested leaves should be positioned correctly
  let leaf32 := result.get! 32
  let leaf42 := result.get! 42
  shouldSatisfy (leaf32.width > 0) "nested leaf in left column should have width"
  shouldSatisfy (leaf42.width > 0) "nested leaf in right column should have width"

test "nested grids with wrapped flex: TabView inside TabView" := do
  -- This mimics the actual afferent-demos structure:
  -- Main TabView (grid) containing ReactiveShowcase TabView (grid) containing tabViewPanel's TabView (grid)
  -- All three have wrapped flex tab bars in their first row
  -- IMPORTANT: TabViews have flexItem := growing 1 to fill available space

  let growingItem : ItemKind := .flexChild (FlexItem.growing 1)

  -- Inner TabView (like tabViewPanel's demo TabView)
  let innerTabBarProps : FlexContainer := { direction := .row, wrap := .wrap, gap := 4, rowGap := 4 }
  let innerTabBar := LayoutNode.flexBox 300 innerTabBarProps #[
    LayoutNode.leaf 301 (ContentSize.mk' 70 25),
    LayoutNode.leaf 302 (ContentSize.mk' 70 25),
    LayoutNode.leaf 303 (ContentSize.mk' 70 25)
  ]
  let innerDivider := LayoutNode.leaf 310 (ContentSize.mk' 0 1)
  let innerContent := LayoutNode.leaf 320 (ContentSize.mk' 50 40)
  let innerGridProps := GridContainer.withTemplate #[.auto, .fixed (.length 1.0), .fr 1] #[.fr 1]
  let innerTabView := LayoutNode.gridBox 200 innerGridProps #[innerTabBar, innerDivider, innerContent] {} growingItem

  -- Middle TabView (like ReactiveShowcase's TabView)
  -- Its content area contains the inner TabView
  let middleTabBarProps : FlexContainer := { direction := .row, wrap := .wrap, gap := 4, rowGap := 4 }
  let middleTabBar := LayoutNode.flexBox 110 middleTabBarProps #[
    LayoutNode.leaf 111 (ContentSize.mk' 80 30),
    LayoutNode.leaf 112 (ContentSize.mk' 80 30),
    LayoutNode.leaf 113 (ContentSize.mk' 80 30),
    LayoutNode.leaf 114 (ContentSize.mk' 80 30),
    LayoutNode.leaf 115 (ContentSize.mk' 80 30),
    LayoutNode.leaf 116 (ContentSize.mk' 80 30),
    LayoutNode.leaf 117 (ContentSize.mk' 80 30),
    LayoutNode.leaf 118 (ContentSize.mk' 80 30)
  ]
  let middleDivider := LayoutNode.leaf 120 (ContentSize.mk' 0 1)
  -- Content panel contains a column with the inner TabView (column also grows)
  let middleContentInner := LayoutNode.column 130 #[innerTabView] (gap := 8) {} growingItem
  let middleGridProps := GridContainer.withTemplate #[.auto, .fixed (.length 1.0), .fr 1] #[.fr 1]
  let middleTabView := LayoutNode.gridBox 100 middleGridProps #[middleTabBar, middleDivider, middleContentInner] {} growingItem

  -- Outer TabView (like main app's TabView)
  let outerTabBarProps : FlexContainer := { direction := .row, wrap := .wrap, gap := 4, rowGap := 4 }
  let outerTabBar := LayoutNode.flexBox 11 outerTabBarProps #[
    LayoutNode.leaf 12 (ContentSize.mk' 70 30),
    LayoutNode.leaf 13 (ContentSize.mk' 70 30),
    LayoutNode.leaf 14 (ContentSize.mk' 70 30),
    LayoutNode.leaf 15 (ContentSize.mk' 70 30),
    LayoutNode.leaf 16 (ContentSize.mk' 70 30),
    LayoutNode.leaf 17 (ContentSize.mk' 70 30),
    LayoutNode.leaf 18 (ContentSize.mk' 70 30),
    LayoutNode.leaf 19 (ContentSize.mk' 70 30)
  ]
  let outerDivider := LayoutNode.leaf 20 (ContentSize.mk' 0 1)
  -- Content contains the middle TabView (column also grows)
  let outerContentInner := LayoutNode.column 30 #[middleTabView] (gap := 8) {} growingItem
  let outerGridProps := GridContainer.withTemplate #[.auto, .fixed (.length 1.0), .fr 1] #[.fr 1]
  let outerTabView := LayoutNode.gridBox 10 outerGridProps #[outerTabBar, outerDivider, outerContentInner] {} growingItem

  -- Root column
  let root := LayoutNode.column 0 #[
    LayoutNode.leaf 1 (ContentSize.mk' 100 30),  -- Title
    outerTabView
  ] (gap := 16)

  -- Layout in 500x600 container
  let result := layout root 500 600

  -- Check outer TabView
  let outerTabBarLayout := result.get! 11
  let outerDividerLayout := result.get! 20
  let outerContentLayout := result.get! 30

  -- Outer tab bar should wrap (8 tabs x 70px = 560 > 500)
  shouldSatisfy (outerTabBarLayout.height > 30) "outer tab bar should wrap to multiple lines"

  -- Outer divider should be after outer tab bar
  shouldBeNear outerDividerLayout.y (outerTabBarLayout.y + outerTabBarLayout.height) 1.0

  -- Outer content should be after outer divider
  shouldSatisfy (outerContentLayout.y > outerDividerLayout.y)
    "outer content should be below outer divider"

  -- Check middle TabView
  let middleTabViewLayout := result.get! 100
  let middleTabBarLayout := result.get! 110
  let middleDividerLayout := result.get! 120
  let middleContentLayout := result.get! 130

  -- Middle TabView should be positioned within outer content
  shouldSatisfy (middleTabViewLayout.y >= outerContentLayout.y)
    "middle TabView should be within outer content area"

  -- Middle tab bar should also wrap
  shouldSatisfy (middleTabBarLayout.height > 30) "middle tab bar should wrap to multiple lines"

  -- Middle content should be after middle divider (not overlapping)
  shouldSatisfy (middleContentLayout.y > middleDividerLayout.y)
    "middle content should be below middle divider"

  -- Check inner TabView
  let innerTabViewLayout := result.get! 200
  let innerTabBarLayout := result.get! 300
  let innerDividerLayout := result.get! 310
  let innerContentLayout := result.get! 320

  -- Inner TabView should be within middle content
  shouldSatisfy (innerTabViewLayout.y >= middleContentLayout.y)
    "inner TabView should be within middle content area"

  -- Inner content should be after inner divider (not overlapping)
  shouldSatisfy (innerContentLayout.y > innerDividerLayout.y)
    "inner content should be below inner divider"

  -- Debug: print all the heights
  IO.println s!"Outer tab bar: y={outerTabBarLayout.y}, h={outerTabBarLayout.height}"
  IO.println s!"Outer divider: y={outerDividerLayout.y}, h={outerDividerLayout.height}"
  IO.println s!"Outer content: y={outerContentLayout.y}, h={outerContentLayout.height}"
  IO.println s!"Middle TabView: y={middleTabViewLayout.y}, h={middleTabViewLayout.height}"
  IO.println s!"Middle tab bar: y={middleTabBarLayout.y}, h={middleTabBarLayout.height}"
  IO.println s!"Middle divider: y={middleDividerLayout.y}, h={middleDividerLayout.height}"
  IO.println s!"Middle content: y={middleContentLayout.y}, h={middleContentLayout.height}"
  IO.println s!"Inner TabView: y={innerTabViewLayout.y}, h={innerTabViewLayout.height}"
  IO.println s!"Inner tab bar: y={innerTabBarLayout.y}, h={innerTabBarLayout.height}"
  IO.println s!"Inner divider: y={innerDividerLayout.y}, h={innerDividerLayout.height}"
  IO.println s!"Inner content: y={innerContentLayout.y}, h={innerContentLayout.height}"

  -- Most importantly: inner content should have positive dimensions
  shouldSatisfy (innerContentLayout.width > 0) "inner content should have positive width"
  shouldSatisfy (innerContentLayout.height > 0) "inner content should have positive height"

  -- The innermost content leaf should be visible
  shouldSatisfy (innerContentLayout.y + innerContentLayout.height <= 600)
    "inner content should be within viewport"


end TrellisTests.LayoutTests.Grid
