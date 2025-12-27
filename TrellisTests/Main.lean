/-
  Trellis Layout Tests
  Unit tests for the CSS Flexbox and Grid layout system.
-/
import Crucible
import Trellis

namespace TrellisTests

open Crucible
open Trellis

testSuite "Trellis Layout Tests"

/-! ## Basic Layout Tests -/

test "single leaf node takes available space" := do
  let node := LayoutNode.leaf 1 ⟨100, 50⟩
  let result := layout node 400 300
  let cl := result.get! 1
  shouldBeNear cl.width 100 0.01
  shouldBeNear cl.height 50 0.01

test "leaf respects fixed width/height constraints" := do
  let node := LayoutNode.leaf 1 ⟨100, 50⟩
    (BoxConstraints.fixed 200 100)
  let result := layout node 400 300
  let cl := result.get! 1
  shouldBeNear cl.width 200 0.01
  shouldBeNear cl.height 100 0.01

/-! ## Flex Row Tests -/

test "flex row places items horizontally" := do
  let node := LayoutNode.row 0 #[
    LayoutNode.leaf 1 ⟨50, 30⟩,
    LayoutNode.leaf 2 ⟨60, 30⟩,
    LayoutNode.leaf 3 ⟨70, 30⟩
  ]
  let result := layout node 400 100
  let cl1 := result.get! 1
  let cl2 := result.get! 2
  let cl3 := result.get! 3
  -- Items should be placed left to right
  shouldSatisfy (cl2.x > cl1.x) "item 2 should be right of item 1"
  shouldSatisfy (cl3.x > cl2.x) "item 3 should be right of item 2"
  -- Widths should match content
  shouldBeNear cl1.width 50 0.01
  shouldBeNear cl2.width 60 0.01
  shouldBeNear cl3.width 70 0.01

test "flex row respects gap" := do
  let node := LayoutNode.row 0 #[
    LayoutNode.leaf 1 ⟨50, 30⟩,
    LayoutNode.leaf 2 ⟨50, 30⟩
  ] (gap := 20)
  let result := layout node 400 100
  let cl1 := result.get! 1
  let cl2 := result.get! 2
  -- Second item should start after first + gap
  let expectedX2 := cl1.x + cl1.width + 20
  shouldBeNear cl2.x expectedX2 0.01

test "flex-grow: 1 distributes space equally" := do
  let node := LayoutNode.flexBox 0 (FlexContainer.row) #[
    LayoutNode.leaf' 1 0 30 {} (.flexChild (FlexItem.growing 1)),
    LayoutNode.leaf' 2 0 30 {} (.flexChild (FlexItem.growing 1))
  ]
  let result := layout node 200 100
  let cl1 := result.get! 1
  let cl2 := result.get! 2
  -- Both should get equal width (200/2 = 100)
  shouldBeNear cl1.width 100 0.01
  shouldBeNear cl2.width 100 0.01

test "flex-grow distributes space proportionally" := do
  let node := LayoutNode.flexBox 0 (FlexContainer.row) #[
    LayoutNode.leaf' 1 0 30 {} (.flexChild (FlexItem.growing 1)),
    LayoutNode.leaf' 2 0 30 {} (.flexChild (FlexItem.growing 2))
  ]
  let result := layout node 300 100
  let cl1 := result.get! 1
  let cl2 := result.get! 2
  -- 1:2 ratio, so 100 and 200
  shouldBeNear cl1.width 100 0.01
  shouldBeNear cl2.width 200 0.01

test "flex-grow distributes remaining space after basis" := do
  let node := LayoutNode.flexBox 0 (FlexContainer.row) #[
    LayoutNode.leaf' 1 50 30 {} (.flexChild { grow := 1, basis := .length 50 }),
    LayoutNode.leaf' 2 50 30 {} (.flexChild { grow := 1, basis := .length 50 })
  ]
  let result := layout node 300 100
  let cl1 := result.get! 1
  let cl2 := result.get! 2
  -- Basis = 100 total, remaining = 200, split equally
  shouldBeNear cl1.width 150 0.01
  shouldBeNear cl2.width 150 0.01

/-! ## Flex Column Tests -/

test "flex column places items vertically" := do
  let node := LayoutNode.column 0 #[
    LayoutNode.leaf 1 ⟨100, 40⟩,
    LayoutNode.leaf 2 ⟨100, 50⟩,
    LayoutNode.leaf 3 ⟨100, 60⟩
  ]
  let result := layout node 200 400
  let cl1 := result.get! 1
  let cl2 := result.get! 2
  let cl3 := result.get! 3
  -- Items should be placed top to bottom
  shouldSatisfy (cl2.y > cl1.y) "item 2 should be below item 1"
  shouldSatisfy (cl3.y > cl2.y) "item 3 should be below item 2"
  -- Heights should match content
  shouldBeNear cl1.height 40 0.01
  shouldBeNear cl2.height 50 0.01
  shouldBeNear cl3.height 60 0.01

/-! ## Justify Content Tests -/

test "justify-content: center centers items" := do
  let props := { FlexContainer.row with justifyContent := .center }
  let node := LayoutNode.flexBox 0 props #[
    LayoutNode.leaf 1 ⟨100, 30⟩
  ]
  let result := layout node 400 100
  let cl := result.get! 1
  -- Item should be centered: (400 - 100) / 2 = 150
  shouldBeNear cl.x 150 0.01

test "justify-content: flex-end aligns to end" := do
  let props := { FlexContainer.row with justifyContent := .flexEnd }
  let node := LayoutNode.flexBox 0 props #[
    LayoutNode.leaf 1 ⟨100, 30⟩
  ]
  let result := layout node 400 100
  let cl := result.get! 1
  -- Item should be at right: 400 - 100 = 300
  shouldBeNear cl.x 300 0.01

test "justify-content: space-between distributes space between items" := do
  let props := { FlexContainer.row with justifyContent := .spaceBetween }
  let node := LayoutNode.flexBox 0 props #[
    LayoutNode.leaf 1 ⟨50, 30⟩,
    LayoutNode.leaf 2 ⟨50, 30⟩,
    LayoutNode.leaf 3 ⟨50, 30⟩
  ]
  let result := layout node 400 100
  let cl1 := result.get! 1
  let cl2 := result.get! 2
  let cl3 := result.get! 3
  -- First at start, last at end
  shouldBeNear cl1.x 0 0.01
  shouldBeNear (cl3.x + cl3.width) 400 0.01
  -- Middle is centered between gaps
  let gap := (400 - 150) / 2  -- 125
  shouldBeNear cl2.x (50 + gap) 0.01

/-! ## Align Items Tests -/

test "align-items: center centers items on cross axis" := do
  let props := { FlexContainer.row with alignItems := .center }
  let node := LayoutNode.flexBox 0 props #[
    LayoutNode.leaf 1 ⟨50, 30⟩
  ]
  let result := layout node 200 100
  let cl := result.get! 1
  -- Item should be vertically centered: (100 - 30) / 2 = 35
  shouldBeNear cl.y 35 0.01

test "align-items: flex-end aligns items to cross end" := do
  let props := { FlexContainer.row with alignItems := .flexEnd }
  let node := LayoutNode.flexBox 0 props #[
    LayoutNode.leaf 1 ⟨50, 30⟩
  ]
  let result := layout node 200 100
  let cl := result.get! 1
  -- Item should be at bottom: 100 - 30 = 70
  shouldBeNear cl.y 70 0.01

test "align-items: stretch makes items fill cross axis" := do
  let props := { FlexContainer.row with alignItems := .stretch }
  let node := LayoutNode.flexBox 0 props #[
    LayoutNode.leaf 1 ⟨50, 30⟩
  ]
  let result := layout node 200 100
  let cl := result.get! 1
  -- Item should stretch to fill height
  shouldBeNear cl.y 0 0.01
  shouldBeNear cl.height 100 0.01

/-! ## Nested Layout Tests -/

test "nested flex containers layout correctly" := do
  let inner := LayoutNode.column 10 #[
    LayoutNode.leaf 11 ⟨40, 20⟩,
    LayoutNode.leaf 12 ⟨40, 20⟩
  ]
  let outer := LayoutNode.row 0 #[
    LayoutNode.leaf 1 ⟨50, 50⟩,
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

/-! ## Edge Case Tests -/

test "empty container produces no child layouts" := do
  let node := LayoutNode.row 0 #[]
  let result := layout node 200 100
  -- Only the container itself
  result.size ≡ 1

test "space-between with single item places at start" := do
  let props := { FlexContainer.row with justifyContent := .spaceBetween }
  let node := LayoutNode.flexBox 0 props #[
    LayoutNode.leaf 1 ⟨100, 30⟩
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

/-! ## Flex Wrap Tests -/

test "flex-wrap: wrap creates multiple lines" := do
  let props := { FlexContainer.row with wrap := .wrap }
  let node := LayoutNode.flexBox 0 props #[
    LayoutNode.leaf 1 ⟨60, 30⟩,
    LayoutNode.leaf 2 ⟨60, 30⟩,
    LayoutNode.leaf 3 ⟨60, 30⟩
  ]
  -- Container is 100px wide, items are 60px each, so they must wrap
  let result := layout node 100 200
  let cl1 := result.get! 1
  let cl2 := result.get! 2
  let cl3 := result.get! 3
  -- Each item on its own line (stacked vertically)
  shouldBeNear cl1.y 0 0.01
  shouldSatisfy (cl2.y > cl1.y) "item 2 should be on a new line"
  shouldSatisfy (cl3.y > cl2.y) "item 3 should be on a new line"

test "flex-wrap: wrap-reverse positions first line at bottom" := do
  let props := { FlexContainer.row with wrap := .wrapReverse }
  let node := LayoutNode.flexBox 0 props #[
    LayoutNode.leaf 1 ⟨60, 30⟩,
    LayoutNode.leaf 2 ⟨60, 30⟩
  ]
  -- Container is 100px wide, items are 60px each, so they wrap
  let result := layout node 100 200
  let cl1 := result.get! 1
  let cl2 := result.get! 2
  -- With wrap-reverse, first line (item 1) should be at the bottom
  -- Item 2 wraps to a line above item 1
  shouldSatisfy (cl1.y > cl2.y) "first line should be below second line in wrap-reverse"

test "flex-wrap: wrap-reverse with column direction" := do
  let props := { FlexContainer.column with wrap := .wrapReverse }
  let node := LayoutNode.flexBox 0 props #[
    LayoutNode.leaf 1 ⟨30, 60⟩,
    LayoutNode.leaf 2 ⟨30, 60⟩
  ]
  -- Container is 100px tall, items are 60px each, so they wrap
  let result := layout node 200 100
  let cl1 := result.get! 1
  let cl2 := result.get! 2
  -- With column wrap-reverse, first column should be at the right
  shouldSatisfy (cl1.x > cl2.x) "first column should be right of second in wrap-reverse"

test "flex-wrap: nowrap keeps all items on one line" := do
  let props := { FlexContainer.row with wrap := .nowrap }
  let node := LayoutNode.flexBox 0 props #[
    LayoutNode.leaf 1 ⟨60, 30⟩,
    LayoutNode.leaf 2 ⟨60, 30⟩,
    LayoutNode.leaf 3 ⟨60, 30⟩
  ]
  -- Container is 100px wide, items are 60px each, but nowrap forces single line
  let result := layout node 100 200
  let cl1 := result.get! 1
  let cl2 := result.get! 2
  let cl3 := result.get! 3
  -- All items should be on the same horizontal line (same y)
  shouldBeNear cl1.y cl2.y 0.01
  shouldBeNear cl2.y cl3.y 0.01

test "flex-wrap with row-gap spaces lines correctly" := do
  let props := { FlexContainer.row with wrap := .wrap, rowGap := 20 }
  let node := LayoutNode.flexBox 0 props #[
    LayoutNode.leaf 1 ⟨100, 30⟩,
    LayoutNode.leaf 2 ⟨100, 30⟩
  ]
  -- Each item takes full width, so they wrap
  let result := layout node 100 200
  let cl1 := result.get! 1
  let cl2 := result.get! 2
  -- Second line should start after first line + rowGap
  let expectedY2 := cl1.y + cl1.height + 20
  shouldBeNear cl2.y expectedY2 0.01

test "flex-wrap: align-content center with multiple lines" := do
  let props := { FlexContainer.row with
    wrap := .wrap
    alignContent := .center
  }
  let node := LayoutNode.flexBox 0 props #[
    LayoutNode.leaf 1 ⟨100, 30⟩,
    LayoutNode.leaf 2 ⟨100, 30⟩
  ]
  -- Each item takes full width (100px), container is 100px, so they wrap
  -- Total content height = 60, container = 200, free space = 140
  -- Center offset should be 70
  let result := layout node 100 200
  let cl1 := result.get! 1
  shouldBeNear cl1.y 70 0.01

test "flex-wrap: align-content space-between with multiple lines" := do
  let props := { FlexContainer.row with
    wrap := .wrap
    alignContent := .spaceBetween
  }
  let node := LayoutNode.flexBox 0 props #[
    LayoutNode.leaf 1 ⟨100, 30⟩,
    LayoutNode.leaf 2 ⟨100, 30⟩
  ]
  -- Each item takes full width, forcing wrap
  let result := layout node 100 200
  let cl1 := result.get! 1
  let cl2 := result.get! 2
  -- First line at top, second line at bottom
  shouldBeNear cl1.y 0 0.01
  shouldBeNear (cl2.y + cl2.height) 200 0.01

test "flex-wrap: single line with wrap-reverse positions correctly" := do
  -- Use flexStart alignContent so line positioning matters
  -- With wrap-reverse, flexStart maps to cross-end (bottom)
  let props := { FlexContainer.row with
    wrap := .wrapReverse
    alignContent := .flexStart
  }
  let node := LayoutNode.flexBox 0 props #[
    LayoutNode.leaf 1 ⟨30, 30⟩,
    LayoutNode.leaf 2 ⟨30, 30⟩
  ]
  -- Items fit on one line (60px < 100px)
  let result := layout node 100 100
  let cl1 := result.get! 1
  let cl2 := result.get! 2
  -- Single line should be at the cross-end (bottom) because wrap-reverse
  -- flips flexStart to mean cross-end
  shouldSatisfy (cl1.y > 0) "single wrap-reverse line should be at bottom"
  shouldBeNear cl1.y cl2.y 0.01

test "flex-wrap: items are sized correctly on each line" := do
  let props := { FlexContainer.row with wrap := .wrap }
  let node := LayoutNode.flexBox 0 props #[
    LayoutNode.leaf 1 ⟨80, 30⟩,
    LayoutNode.leaf 2 ⟨80, 30⟩
  ]
  -- Container 100px, items 80px each, so they wrap
  let result := layout node 100 200
  let cl1 := result.get! 1
  let cl2 := result.get! 2
  -- Each item should maintain its width
  shouldBeNear cl1.width 80 0.01
  shouldBeNear cl2.width 80 0.01

/-! ## Flex Constraint Resolution Tests -/

test "flex-grow: item hitting max-width redistributes to others" := do
  -- Item 1 can grow up to 80px max, item 2 has no limit
  -- In 300px container with both items having grow:1, naive split would be 150px each
  -- But item 1 is capped at 80px, so remaining 220px goes to item 2
  let node := LayoutNode.flexBox 0 (FlexContainer.row) #[
    LayoutNode.leaf' 1 0 30 { maxWidth := some 80 } (.flexChild (FlexItem.growing 1)),
    LayoutNode.leaf' 2 0 30 {} (.flexChild (FlexItem.growing 1))
  ]
  let result := layout node 300 100
  let cl1 := result.get! 1
  let cl2 := result.get! 2
  -- Item 1 should be capped at 80px
  shouldBeNear cl1.width 80 0.01
  -- Item 2 gets the rest: 300 - 80 = 220px
  shouldBeNear cl2.width 220 0.01

test "flex-grow: multiple items hitting max-width" := do
  -- Both items have max-width of 80px, third item gets remaining space
  -- Container 300px, all grow:1, naive split 100px each
  -- Items 1 and 2 cap at 80, item 3 gets 300 - 80 - 80 = 140px
  let node := LayoutNode.flexBox 0 (FlexContainer.row) #[
    LayoutNode.leaf' 1 0 30 { maxWidth := some 80 } (.flexChild (FlexItem.growing 1)),
    LayoutNode.leaf' 2 0 30 { maxWidth := some 80 } (.flexChild (FlexItem.growing 1)),
    LayoutNode.leaf' 3 0 30 {} (.flexChild (FlexItem.growing 1))
  ]
  let result := layout node 300 100
  let cl1 := result.get! 1
  let cl2 := result.get! 2
  let cl3 := result.get! 3
  shouldBeNear cl1.width 80 0.01
  shouldBeNear cl2.width 80 0.01
  shouldBeNear cl3.width 140 0.01

test "flex-shrink: item hitting min-width redistributes to others" := do
  -- Items with flex-basis 100px each in a 100px container need to shrink
  -- Item 1 has min-width 80px, item 2 has no limit
  -- Total basis 200px, need to shrink 100px
  -- Naive split with shrink:1 would shrink each by 50px to 50px
  -- But item 1 can only shrink to 80px (20px shrink), so item 2 shrinks 80px to 20px
  let node := LayoutNode.flexBox 0 (FlexContainer.row) #[
    LayoutNode.leaf' 1 100 30 { minWidth := 80 } (.flexChild { shrink := 1, basis := .length 100 }),
    LayoutNode.leaf' 2 100 30 {} (.flexChild { shrink := 1, basis := .length 100 })
  ]
  let result := layout node 100 100
  let cl1 := result.get! 1
  let cl2 := result.get! 2
  -- Item 1 capped at min 80px
  shouldBeNear cl1.width 80 0.01
  -- Item 2 gets the rest: 100 - 80 = 20px
  shouldBeNear cl2.width 20 0.01

test "flex-shrink: multiple items hitting min-width" := do
  -- Three items with flex-basis 100px each in a 120px container
  -- Items 1 and 2 have min-width 50px, item 3 has no limit
  -- Total basis 300px, need to shrink 180px total
  let node := LayoutNode.flexBox 0 (FlexContainer.row) #[
    LayoutNode.leaf' 1 100 30 { minWidth := 50 } (.flexChild { shrink := 1, basis := .length 100 }),
    LayoutNode.leaf' 2 100 30 { minWidth := 50 } (.flexChild { shrink := 1, basis := .length 100 }),
    LayoutNode.leaf' 3 100 30 {} (.flexChild { shrink := 1, basis := .length 100 })
  ]
  let result := layout node 120 100
  let cl1 := result.get! 1
  let cl2 := result.get! 2
  let cl3 := result.get! 3
  -- Items 1 and 2 capped at min 50px
  shouldBeNear cl1.width 50 0.01
  shouldBeNear cl2.width 50 0.01
  -- Item 3 gets the rest: 120 - 50 - 50 = 20px
  shouldBeNear cl3.width 20 0.01

test "flex-grow: different grow factors with max-width constraint" := do
  -- Item 1: grow 2, max 120px; Item 2: grow 1, no max
  -- Container 300px, total grow 3
  -- Naive: Item 1 gets 200px (capped to 120), Item 2 gets 100px
  -- After cap: Item 2 gets remaining 300 - 120 = 180px
  let node := LayoutNode.flexBox 0 (FlexContainer.row) #[
    LayoutNode.leaf' 1 0 30 { maxWidth := some 120 } (.flexChild (FlexItem.growing 2)),
    LayoutNode.leaf' 2 0 30 {} (.flexChild (FlexItem.growing 1))
  ]
  let result := layout node 300 100
  let cl1 := result.get! 1
  let cl2 := result.get! 2
  shouldBeNear cl1.width 120 0.01
  shouldBeNear cl2.width 180 0.01

/-! ## Grid Basic Tests -/

test "grid with 3 equal fr columns" := do
  let props := GridContainer.columns 3
  let node := LayoutNode.gridBox 0 props #[
    LayoutNode.leaf 1 ⟨0, 30⟩,
    LayoutNode.leaf 2 ⟨0, 30⟩,
    LayoutNode.leaf 3 ⟨0, 30⟩
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
    LayoutNode.leaf 1 ⟨0, 30⟩,
    LayoutNode.leaf 2 ⟨0, 30⟩,
    LayoutNode.leaf 3 ⟨0, 30⟩
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
    LayoutNode.leaf 1 ⟨0, 30⟩,
    LayoutNode.leaf 2 ⟨0, 30⟩,
    LayoutNode.leaf 3 ⟨0, 30⟩,
    LayoutNode.leaf 4 ⟨0, 30⟩
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
    LayoutNode.leaf 1 ⟨0, 30⟩,
    LayoutNode.leaf 2 ⟨0, 30⟩,
    LayoutNode.leaf 3 ⟨0, 30⟩,
    LayoutNode.leaf 4 ⟨0, 30⟩,
    LayoutNode.leaf 5 ⟨0, 30⟩
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

/-! ## Grid Spanning Tests -/

test "grid item spanning multiple columns" := do
  let props := GridContainer.columns 3
  let spanItem := { GridItem.default with
    placement := { column := GridSpan.spanTracks 2 }
  }
  let node := LayoutNode.gridBox 0 props #[
    LayoutNode.leaf' 1 0 30 {} (.gridChild spanItem),  -- spans 2 cols
    LayoutNode.leaf 2 ⟨0, 30⟩
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
    LayoutNode.leaf 2 ⟨0, 50⟩,
    LayoutNode.leaf 3 ⟨0, 50⟩
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
    LayoutNode.leaf 1 ⟨50, 30⟩
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
    LayoutNode.leaf 1 ⟨50, 30⟩
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
    LayoutNode.leaf 1 ⟨30, 20⟩
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
    LayoutNode.leaf 1 ⟨50, 30⟩
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
    LayoutNode.leaf 1 ⟨0, 50⟩,
    LayoutNode.leaf 2 ⟨0, 50⟩,
    LayoutNode.leaf 3 ⟨0, 50⟩,
    LayoutNode.leaf 4 ⟨0, 50⟩
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
    LayoutNode.leaf 1 ⟨200, 30⟩,  -- Content wants 200px but max is 150
    LayoutNode.leaf 2 ⟨0, 30⟩
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
    LayoutNode.leaf 1 ⟨20, 30⟩,  -- Content wants only 20px but min is 100
    LayoutNode.leaf 2 ⟨0, 30⟩
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
    LayoutNode.leaf 1 ⟨0, 30⟩,
    LayoutNode.leaf 2 ⟨0, 30⟩
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
    LayoutNode.leaf 1 ⟨0, 30⟩,
    LayoutNode.leaf 2 ⟨0, 30⟩
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
    LayoutNode.leaf 1 ⟨50, 30⟩   -- Content height 30 < min 50
  ]
  let result := layout node 200 200
  let cl := result.get! 1
  -- Row should be at least 50 (min)
  shouldSatisfy (cl.height >= 50) "row height should be at least minmax min"

test "grid mixed fixed and minmax columns" := do
  -- Fixed 50px + minmax(50, 100) + fr
  let props := GridContainer.withColumns #[.px 50, .minmax (.px 50) (.px 100), .fr 1]
  let node := LayoutNode.gridBox 0 props #[
    LayoutNode.leaf 1 ⟨0, 30⟩,
    LayoutNode.leaf 2 ⟨80, 30⟩,  -- Content 80 within minmax range
    LayoutNode.leaf 3 ⟨0, 30⟩
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
    LayoutNode.leaf 1 ⟨0, 30⟩,
    LayoutNode.leaf 2 ⟨0, 30⟩,
    LayoutNode.leaf 3 ⟨0, 30⟩
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
    LayoutNode.leaf 1 ⟨0, 30⟩,
    LayoutNode.leaf 2 ⟨0, 30⟩,
    LayoutNode.leaf 3 ⟨0, 30⟩,
    LayoutNode.leaf 4 ⟨0, 30⟩
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
    LayoutNode.leaf 1 ⟨0, 30⟩,
    LayoutNode.leaf 2 ⟨0, 30⟩,
    LayoutNode.leaf 3 ⟨0, 30⟩
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
    LayoutNode.leaf 1 ⟨0, 30⟩,
    LayoutNode.leaf 2 ⟨0, 30⟩
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
    LayoutNode.leaf 1 ⟨0, 30⟩,
    LayoutNode.leaf 2 ⟨0, 30⟩
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
    LayoutNode.leaf 1 ⟨0, 30⟩,
    LayoutNode.leaf 2 ⟨0, 30⟩
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
    LayoutNode.leaf 1 ⟨0, 0⟩,
    LayoutNode.leaf 2 ⟨0, 0⟩,
    LayoutNode.leaf 3 ⟨0, 0⟩
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

#generate_tests

end TrellisTests

def main : IO UInt32 := do
  runAllSuites
