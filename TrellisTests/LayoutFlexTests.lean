/-
  Trellis Layout Tests - Flex
  Unit tests for the CSS Flexbox layout, including row/column direction,
  wrapping, alignment, and flex-grow/shrink dynamics.
-/
import Crucible
import Trellis

namespace TrellisTests.LayoutTests.Flex

open Crucible
open Trellis

testSuite "Trellis Layout Tests - Flex"

/-! ## Flex Row Tests -/

test "flex row places items horizontally" := do
  let node := LayoutNode.row 0 #[
    LayoutNode.leaf 1 (ContentSize.mk' 50 30),
    LayoutNode.leaf 2 (ContentSize.mk' 60 30),
    LayoutNode.leaf 3 (ContentSize.mk' 70 30)
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
    LayoutNode.leaf 1 (ContentSize.mk' 50 30),
    LayoutNode.leaf 2 (ContentSize.mk' 50 30)
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
    LayoutNode.leaf 1 (ContentSize.mk' 100 40),
    LayoutNode.leaf 2 (ContentSize.mk' 100 50),
    LayoutNode.leaf 3 (ContentSize.mk' 100 60)
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
    LayoutNode.leaf 1 (ContentSize.mk' 100 30)
  ]
  let result := layout node 400 100
  let cl := result.get! 1
  -- Item should be centered: (400 - 100) / 2 = 150
  shouldBeNear cl.x 150 0.01

test "justify-content: flex-end aligns to end" := do
  let props := { FlexContainer.row with justifyContent := .flexEnd }
  let node := LayoutNode.flexBox 0 props #[
    LayoutNode.leaf 1 (ContentSize.mk' 100 30)
  ]
  let result := layout node 400 100
  let cl := result.get! 1
  -- Item should be at right: 400 - 100 = 300
  shouldBeNear cl.x 300 0.01

test "justify-content: space-between distributes space between items" := do
  let props := { FlexContainer.row with justifyContent := .spaceBetween }
  let node := LayoutNode.flexBox 0 props #[
    LayoutNode.leaf 1 (ContentSize.mk' 50 30),
    LayoutNode.leaf 2 (ContentSize.mk' 50 30),
    LayoutNode.leaf 3 (ContentSize.mk' 50 30)
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
    LayoutNode.leaf 1 (ContentSize.mk' 50 30)
  ]
  let result := layout node 200 100
  let cl := result.get! 1
  -- Item should be vertically centered: (100 - 30) / 2 = 35
  shouldBeNear cl.y 35 0.01

test "align-items: flex-end aligns items to cross end" := do
  let props := { FlexContainer.row with alignItems := .flexEnd }
  let node := LayoutNode.flexBox 0 props #[
    LayoutNode.leaf 1 (ContentSize.mk' 50 30)
  ]
  let result := layout node 200 100
  let cl := result.get! 1
  -- Item should be at bottom: 100 - 30 = 70
  shouldBeNear cl.y 70 0.01

test "align-items: stretch makes items fill cross axis" := do
  let props := { FlexContainer.row with alignItems := .stretch }
  let node := LayoutNode.flexBox 0 props #[
    LayoutNode.leaf 1 (ContentSize.mk' 50 30)
  ]
  let result := layout node 200 100
  let cl := result.get! 1
  -- Item should stretch to fill height
  shouldBeNear cl.y 0 0.01
  shouldBeNear cl.height 100 0.01

/-! ## Flex Wrap Tests -/

test "flex-wrap: wrap creates multiple lines" := do
  let props := { FlexContainer.row with wrap := .wrap }
  let node := LayoutNode.flexBox 0 props #[
    LayoutNode.leaf 1 (ContentSize.mk' 60 30),
    LayoutNode.leaf 2 (ContentSize.mk' 60 30),
    LayoutNode.leaf 3 (ContentSize.mk' 60 30)
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
    LayoutNode.leaf 1 (ContentSize.mk' 60 30),
    LayoutNode.leaf 2 (ContentSize.mk' 60 30)
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
    LayoutNode.leaf 1 (ContentSize.mk' 30 60),
    LayoutNode.leaf 2 (ContentSize.mk' 30 60)
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
    LayoutNode.leaf 1 (ContentSize.mk' 60 30),
    LayoutNode.leaf 2 (ContentSize.mk' 60 30),
    LayoutNode.leaf 3 (ContentSize.mk' 60 30)
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
    LayoutNode.leaf 1 (ContentSize.mk' 100 30),
    LayoutNode.leaf 2 (ContentSize.mk' 100 30)
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
    LayoutNode.leaf 1 (ContentSize.mk' 100 30),
    LayoutNode.leaf 2 (ContentSize.mk' 100 30)
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
    LayoutNode.leaf 1 (ContentSize.mk' 100 30),
    LayoutNode.leaf 2 (ContentSize.mk' 100 30)
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
    LayoutNode.leaf 1 (ContentSize.mk' 30 30),
    LayoutNode.leaf 2 (ContentSize.mk' 30 30)
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
    LayoutNode.leaf 1 (ContentSize.mk' 80 30),
    LayoutNode.leaf 2 (ContentSize.mk' 80 30)
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

/-! ## Baseline Alignment Tests -/

test "flex baseline: items with same baseline align" := do
  -- Two items with different heights but same baseline
  -- Item 1: 40px tall, baseline at 30px (10px below baseline)
  -- Item 2: 60px tall, baseline at 30px (30px below baseline)
  let props := { FlexContainer.row with alignItems := .baseline }
  let node := LayoutNode.flexBox 0 props #[
    LayoutNode.leaf 1 (ContentSize.withBaseline 50 40 30),
    LayoutNode.leaf 2 (ContentSize.withBaseline 50 60 30)
  ]
  let result := layout node 200 100
  let cl1 := result.get! 1
  let cl2 := result.get! 2
  -- Both items should have same baseline position (y + baseline should be equal)
  let baseline1 := cl1.y + 30
  let baseline2 := cl2.y + 30
  shouldBeNear baseline1 baseline2 0.01

test "flex baseline: different baselines align at max" := do
  -- Item 1: baseline at 20px from top
  -- Item 2: baseline at 40px from top
  -- The line's max baseline should be 40px, so item 1 is pushed down by 20px
  let props := { FlexContainer.row with alignItems := .baseline }
  let node := LayoutNode.flexBox 0 props #[
    LayoutNode.leaf 1 (ContentSize.withBaseline 50 50 20),
    LayoutNode.leaf 2 (ContentSize.withBaseline 50 50 40)
  ]
  let result := layout node 200 100
  let cl1 := result.get! 1
  let cl2 := result.get! 2
  -- Item 2 has larger baseline, so it starts at y=0
  -- Item 1 is pushed down so baselines align
  -- Item 2's baseline from container top = 0 + 40 = 40
  -- Item 1's baseline from container top = (40-20) + 20 = 40
  let baseline1 := cl1.y + 20
  let baseline2 := cl2.y + 40
  shouldBeNear baseline1 baseline2 0.01
  shouldSatisfy (cl1.y > cl2.y) "item 1 should be pushed down"

test "flex baseline: default baseline is height" := do
  -- Items without explicit baseline default to height
  let props := { FlexContainer.row with alignItems := .baseline }
  let node := LayoutNode.flexBox 0 props #[
    LayoutNode.leaf 1 (ContentSize.mk' 50 30),  -- baseline defaults to 30
    LayoutNode.leaf 2 (ContentSize.mk' 50 50)   -- baseline defaults to 50
  ]
  let result := layout node 200 100
  let cl1 := result.get! 1
  let cl2 := result.get! 2
  -- Item 2 has larger baseline (50), so baselines align there
  let baseline1 := cl1.y + 30
  let baseline2 := cl2.y + 50
  shouldBeNear baseline1 baseline2 0.01

test "flex baseline: align-self baseline overrides container" := do
  -- Container uses center, but one item uses baseline
  let props := { FlexContainer.row with alignItems := .center }
  let baselineItem := { FlexItem.default with alignSelf := some .baseline }
  let node := LayoutNode.flexBox 0 props #[
    LayoutNode.leaf' 1 50 40 {} (.flexChild baselineItem),
    LayoutNode.leaf 2 (ContentSize.mk' 50 40)  -- uses center
  ]
  let result := layout node 200 100
  let cl1 := result.get! 1
  let cl2 := result.get! 2
  -- Item 2 is centered: (100 - 40) / 2 = 30
  shouldBeNear cl2.y 30 0.01
  -- Item 1 uses baseline alignment (alone, so starts at top)
  -- Note: single baseline item in a line uses its own baseline as max
  shouldBeNear cl1.y 0 0.01

/-! ## Flex Order Tests -/

test "flex order: items reordered by order property" := do
  let props := FlexContainer.row
  let node := LayoutNode.flexBox 0 props #[
    LayoutNode.leaf' 1 50 30 {} (.flexChild { order := 2 }),
    LayoutNode.leaf' 2 50 30 {} (.flexChild { order := 0 }),
    LayoutNode.leaf' 3 50 30 {} (.flexChild { order := 1 })
  ]
  let result := layout node 200 100
  let cl1 := result.get! 1
  let cl2 := result.get! 2
  let cl3 := result.get! 3
  -- Visual order: 2 (order=0), 3 (order=1), 1 (order=2)
  shouldSatisfy (cl2.x < cl3.x) "item 2 should be leftmost"
  shouldSatisfy (cl3.x < cl1.x) "item 3 should be middle"

test "flex order: equal order preserves source order" := do
  let props := FlexContainer.row
  let node := LayoutNode.flexBox 0 props #[
    LayoutNode.leaf' 1 50 30 {} (.flexChild { order := 0 }),
    LayoutNode.leaf' 2 50 30 {} (.flexChild { order := 0 }),
    LayoutNode.leaf' 3 50 30 {} (.flexChild { order := 0 })
  ]
  let result := layout node 200 100
  let cl1 := result.get! 1
  let cl2 := result.get! 2
  let cl3 := result.get! 3
  -- Same order: maintain source order (1, 2, 3)
  shouldSatisfy (cl1.x < cl2.x) "item 1 should be before item 2"
  shouldSatisfy (cl2.x < cl3.x) "item 2 should be before item 3"

test "flex order: negative order values" := do
  let props := FlexContainer.row
  let node := LayoutNode.flexBox 0 props #[
    LayoutNode.leaf' 1 50 30 {} (.flexChild { order := 0 }),
    LayoutNode.leaf' 2 50 30 {} (.flexChild { order := -1 })
  ]
  let result := layout node 200 100
  let cl1 := result.get! 1
  let cl2 := result.get! 2
  -- Item 2 with order=-1 should come before item 1 with order=0
  shouldSatisfy (cl2.x < cl1.x) "item 2 should be before item 1"

#generate_tests

end TrellisTests.LayoutTests.Flex
