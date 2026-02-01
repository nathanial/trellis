/-
  Content Size Regression Tests

  Tests that leaf nodes with content sizes are properly sized in various layouts.
  These tests target the bug where tabs, buttons, and cards appear with zero
  width/height because their content sizes are not being used.
-/
import Crucible
import Trellis

namespace TrellisTests.ContentSizeTests

open Crucible
open Trellis

testSuite "Content Size Tests"

/-! ## Horizontal Row with Content-Sized Tabs

This mimics the TabBar demo pattern: a horizontal row of tabs where each
tab's width should be determined by its text content size.
-/

test "content: single leaf in row uses content width (main axis)" := do
  -- A single leaf with content size 80x24 (like a tab with text)
  -- In a row, main axis (width) should use content width
  -- Cross axis (height) stretches by default (CSS behavior)
  let tab := LayoutNode.leaf 1 (ContentSize.mk' 80 24)
  let row := LayoutNode.row 0 #[tab]
  let result := layout row 500 100
  let tabLayout := result.get! 1
  -- Main axis: uses content width
  shouldBeNear tabLayout.width 80 0.01
  -- Cross axis: stretches to fill (default CSS align-items: stretch)
  shouldBeNear tabLayout.height 100 0.01

test "content: multiple tabs in row each use their content widths (main axis)" := do
  -- Three tabs with different text lengths: "Home" (50), "Settings" (70), "About" (45)
  let tab1 := LayoutNode.leaf 1 (ContentSize.mk' 50 24)
  let tab2 := LayoutNode.leaf 2 (ContentSize.mk' 70 24)
  let tab3 := LayoutNode.leaf 3 (ContentSize.mk' 45 24)
  let row := LayoutNode.row 0 #[tab1, tab2, tab3] (gap := 8)
  let result := layout row 500 100
  -- Each tab retains its content width (main axis)
  let tl1 := result.get! 1
  let tl2 := result.get! 2
  let tl3 := result.get! 3
  shouldBeNear tl1.width 50 0.01
  shouldBeNear tl2.width 70 0.01
  shouldBeNear tl3.width 45 0.01
  -- Heights stretch to row height (cross axis, default CSS behavior)
  shouldBeNear tl1.height 100 0.01
  shouldBeNear tl2.height 100 0.01
  shouldBeNear tl3.height 100 0.01

test "content: tabs positioned correctly with gap" := do
  -- Tab positions should account for widths and gaps
  let tab1 := LayoutNode.leaf 1 (ContentSize.mk' 50 24)
  let tab2 := LayoutNode.leaf 2 (ContentSize.mk' 70 24)
  let tab3 := LayoutNode.leaf 3 (ContentSize.mk' 45 24)
  let row := LayoutNode.row 0 #[tab1, tab2, tab3] (gap := 10)
  let result := layout row 500 100
  let tl1 := result.get! 1
  let tl2 := result.get! 2
  let tl3 := result.get! 3
  -- tab1 at x=0
  shouldBeNear tl1.x 0 0.01
  -- tab2 at x = 50 + 10 = 60
  shouldBeNear tl2.x 60 0.01
  -- tab3 at x = 60 + 70 + 10 = 140
  shouldBeNear tl3.x 140 0.01

/-! ## Buttons with Content

Buttons are typically content-sized text wrapped in a container with padding.
-/

test "content: button text determines button width (main axis)" := do
  -- Button label "Click Me" = 60x20 content
  -- In a row, text width (main axis) is content-based
  let buttonText := LayoutNode.leaf 1 (ContentSize.mk' 60 20)
  let button := LayoutNode.row 0 #[buttonText] (box := {
    padding := Trellis.EdgeInsets.symmetric 16 8  -- horizontal: 16, vertical: 8
  })
  let result := layout button 500 100
  let textLayout := result.get! 1
  -- Main axis (width) uses content size
  shouldBeNear textLayout.width 60 0.01
  -- Cross axis (height) stretches to fill button height minus padding
  -- Button height = 100 (available), content area = 100 - 8*2 = 84
  shouldBeNear textLayout.height 84 0.01

test "content: button row - multiple buttons sized by their labels" := do
  -- Row of buttons with different label lengths
  let btn1Text := LayoutNode.leaf 11 (ContentSize.mk' 40 20)
  let btn2Text := LayoutNode.leaf 21 (ContentSize.mk' 80 20)
  let btn3Text := LayoutNode.leaf 31 (ContentSize.mk' 55 20)
  -- Each button is a row containing its text
  let btn1 := LayoutNode.row 1 #[btn1Text]
  let btn2 := LayoutNode.row 2 #[btn2Text]
  let btn3 := LayoutNode.row 3 #[btn3Text]
  -- Container row with all buttons
  let row := LayoutNode.row 0 #[btn1, btn2, btn3] (gap := 8)
  let result := layout row 500 100
  -- Each button's text should have its content width
  let tl1 := result.get! 11
  let tl2 := result.get! 21
  let tl3 := result.get! 31
  shouldBeNear tl1.width 40 0.01
  shouldBeNear tl2.width 80 0.01
  shouldBeNear tl3.width 55 0.01

test "content: flex container intrinsic includes padding" := do
  -- Padded flex container should report content size including padding
  let text := LayoutNode.leaf 2 (ContentSize.mk' 50 20)
  let padded := LayoutNode.row 1 #[text] (box := {
    padding := Trellis.EdgeInsets.symmetric 10 5
  })
  let parentProps := { FlexContainer.row with alignItems := .flexStart }
  let parent := LayoutNode.flexBox 0 parentProps #[padded]
  let result := layout parent 200 100
  let paddedLayout := result.get! 1
  -- Width = content 50 + horizontal padding 20
  shouldBeNear paddedLayout.width 70 0.01
  -- Height = content 20 + vertical padding 10
  shouldBeNear paddedLayout.height 30 0.01

test "content: grid container intrinsic includes padding" := do
  -- Padded grid container should report content size including padding
  let cell := LayoutNode.leaf 2 (ContentSize.mk' 40 30)
  let gridProps := GridContainer.withTemplate #[.auto] #[.auto]
  let paddedGrid := LayoutNode.gridBox 1 gridProps #[cell] (box := {
    padding := Trellis.EdgeInsets.symmetric 6 4
  })
  let parentProps := { FlexContainer.row with alignItems := .flexStart }
  let parent := LayoutNode.flexBox 0 parentProps #[paddedGrid]
  let result := layout parent 200 100
  let gridLayout := result.get! 1
  -- Width = content 40 + horizontal padding 12
  shouldBeNear gridLayout.width 52 0.01
  -- Height = content 30 + vertical padding 8
  shouldBeNear gridLayout.height 38 0.01

/-! ## Cards with Content-Sized Children

Cards contain various content elements that should size based on their content.
-/

test "content: card with title and icon - main axis sizing" := do
  -- Card structure: column with icon (32x32) and title text (100x18)
  -- In a column, height is main axis, width is cross axis (stretches)
  let icon := LayoutNode.leaf 1 (ContentSize.mk' 32 32)
  let title := LayoutNode.leaf 2 (ContentSize.mk' 100 18)
  let card := LayoutNode.column 0 #[icon, title] (gap := 8)
  let result := layout card 200 300
  let iconLayout := result.get! 1
  let titleLayout := result.get! 2
  -- Cross axis (width) stretches to column width (default CSS behavior)
  shouldBeNear iconLayout.width 200 0.01
  shouldBeNear titleLayout.width 200 0.01
  -- Main axis (height) uses content size
  shouldBeNear iconLayout.height 32 0.01
  shouldBeNear titleLayout.height 18 0.01

test "content: grid of cards - main axis heights preserved" := do
  -- 2x2 grid of cards, each with a shape (40x40) and label (60x16)
  -- In columns within grid cells, height is main axis, width stretches
  let mkCard (shapeId labelId : Nat) : LayoutNode :=
    LayoutNode.column (shapeId * 10) #[
      LayoutNode.leaf shapeId (ContentSize.mk' 40 40),
      LayoutNode.leaf labelId (ContentSize.mk' 60 16)
    ] (gap := 4)
  let grid := LayoutNode.gridBox 0 (GridContainer.columns 2 10) #[
    mkCard 1 2,
    mkCard 3 4,
    mkCard 5 6,
    mkCard 7 8
  ]
  let result := layout grid 400 400
  -- Grid cells are 195x195 (400-10)/2
  -- Column children stretch to fill cell width
  let shape1 := result.get! 1
  let label2 := result.get! 2
  -- Width stretches to fill column (which fills cell)
  shouldBeNear shape1.width 195 0.01
  -- Heights use content size (main axis in column)
  shouldBeNear shape1.height 40 0.01
  shouldBeNear label2.height 16 0.01

/-! ## Nested Containers with Content-Sized Leaves

Test that content sizes propagate correctly through multiple nesting levels.
-/

test "content: deeply nested leaf retains content size" := do
  -- row > column > row > leaf
  let leaf := LayoutNode.leaf 4 (ContentSize.mk' 75 25)
  let innerRow := LayoutNode.row 3 #[leaf]
  let column := LayoutNode.column 2 #[innerRow]
  let outerRow := LayoutNode.row 1 #[column]
  let result := layout outerRow 500 300
  let leafLayout := result.get! 4
  -- Leaf should retain its content size through all nesting
  shouldBeNear leafLayout.width 75 0.01
  shouldBeNear leafLayout.height 25 0.01

test "content: sibling leaves in nested structure all sized by content" := do
  -- column > row > (leaf1, leaf2, leaf3)
  let leaf1 := LayoutNode.leaf 1 (ContentSize.mk' 30 20)
  let leaf2 := LayoutNode.leaf 2 (ContentSize.mk' 50 20)
  let leaf3 := LayoutNode.leaf 3 (ContentSize.mk' 40 20)
  let row := LayoutNode.row 10 #[leaf1, leaf2, leaf3] (gap := 5)
  let column := LayoutNode.column 0 #[row]
  let result := layout column 500 300
  let tl1 := result.get! 1
  let tl2 := result.get! 2
  let tl3 := result.get! 3
  shouldBeNear tl1.width 30 0.01
  shouldBeNear tl2.width 50 0.01
  shouldBeNear tl3.width 40 0.01

/-! ## Intrinsic Size Measurement

Test that measureIntrinsicSize correctly computes content sizes.
-/

test "content: measureIntrinsicSize returns leaf content size" := do
  let leaf := LayoutNode.leaf 1 (ContentSize.mk' 123 45)
  let (w, h) := measureIntrinsicSize leaf
  shouldBeNear w 123 0.01
  shouldBeNear h 45 0.01

test "content: measureIntrinsicSize aggregates children in row" := do
  -- Row of three leaves: 30+50+40 = 120 width, max(20,25,20) = 25 height
  let row := LayoutNode.row 0 #[
    LayoutNode.leaf 1 (ContentSize.mk' 30 20),
    LayoutNode.leaf 2 (ContentSize.mk' 50 25),
    LayoutNode.leaf 3 (ContentSize.mk' 40 20)
  ] (gap := 10)
  let (w, h) := measureIntrinsicSize row
  -- Width = 30 + 50 + 40 + 10*2 (gaps) = 140
  shouldBeNear w 140 0.01
  -- Height = max(20, 25, 20) = 25
  shouldBeNear h 25 0.01

test "content: measureIntrinsicSize aggregates children in column" := do
  -- Column of three leaves: max(30,50,40) = 50 width, 20+25+20+10+10 = 85 height
  let column := LayoutNode.column 0 #[
    LayoutNode.leaf 1 (ContentSize.mk' 30 20),
    LayoutNode.leaf 2 (ContentSize.mk' 50 25),
    LayoutNode.leaf 3 (ContentSize.mk' 40 20)
  ] (gap := 10)
  let (w, h) := measureIntrinsicSize column
  -- Width = max(30, 50, 40) = 50
  shouldBeNear w 50 0.01
  -- Height = 20 + 25 + 20 + 10*2 (gaps) = 85
  shouldBeNear h 85 0.01

/-! ## Zero Content Size Edge Cases

Ensure that zero-sized content doesn't cause issues.
-/

test "content: leaf with zero content size in row" := do
  -- A zero-width leaf shouldn't crash or affect siblings
  let leaf1 := LayoutNode.leaf 1 (ContentSize.mk' 0 0)
  let leaf2 := LayoutNode.leaf 2 (ContentSize.mk' 50 20)
  let row := LayoutNode.row 0 #[leaf1, leaf2] (gap := 5)
  let result := layout row 500 100
  let tl1 := result.get! 1
  let tl2 := result.get! 2
  -- Zero-content leaf: width=0 (main axis), height stretches (cross axis)
  shouldBeNear tl1.width 0 0.01
  shouldBeNear tl1.height 100 0.01  -- Cross axis stretches
  -- Second leaf should be positioned after first + gap
  shouldBeNear tl2.x 5 0.01
  shouldBeNear tl2.width 50 0.01

/-! ## Content with Box Constraints

Content size should interact correctly with box constraints.
-/

test "content: minWidth takes precedence over smaller content" := do
  let leaf := LayoutNode.leaf 1 (ContentSize.mk' 30 20) { minWidth := 50 }
  let result := layout leaf 500 100
  let tl := result.get! 1
  -- minWidth (50) > content (30), so should use minWidth
  shouldBeNear tl.width 50 0.01

test "content: maxWidth limits larger content" := do
  let leaf := LayoutNode.leaf 1 (ContentSize.mk' 100 20) { maxWidth := some 60 }
  let result := layout leaf 500 100
  let tl := result.get! 1
  -- maxWidth (60) < content (100), so should use maxWidth
  shouldBeNear tl.width 60 0.01

test "content: content size used when no constraints specified" := do
  let leaf := LayoutNode.leaf 1 (ContentSize.mk' 77 33)
  let result := layout leaf 500 100
  let tl := result.get! 1
  -- No constraints, should use content size exactly
  shouldBeNear tl.width 77 0.01
  shouldBeNear tl.height 33 0.01

/-! ## Cross-Axis Content Sizing (with align-items: flexStart)

By default, flex items stretch on the cross axis. To use content size on
cross axis, set align-items to flexStart (or center, flexEnd, baseline).
-/

test "content: row with alignItems flexStart - cross axis uses content" := do
  -- With alignItems: flexStart, items don't stretch
  let tab1 := LayoutNode.leaf 1 (ContentSize.mk' 50 24)
  let tab2 := LayoutNode.leaf 2 (ContentSize.mk' 70 30)
  let flexProps := { FlexContainer.row 8 with alignItems := .flexStart }
  let row := LayoutNode.flexBox 0 flexProps #[tab1, tab2]
  let result := layout row 500 100
  let tl1 := result.get! 1
  let tl2 := result.get! 2
  -- Main axis: content width
  shouldBeNear tl1.width 50 0.01
  shouldBeNear tl2.width 70 0.01
  -- Cross axis: content height (no stretch)
  shouldBeNear tl1.height 24 0.01
  shouldBeNear tl2.height 30 0.01

test "content: column with alignItems flexStart - cross axis uses content" := do
  -- With alignItems: flexStart, items don't stretch
  let item1 := LayoutNode.leaf 1 (ContentSize.mk' 80 40)
  let item2 := LayoutNode.leaf 2 (ContentSize.mk' 60 50)
  let flexProps := { FlexContainer.column 8 with alignItems := .flexStart }
  let col := LayoutNode.flexBox 0 flexProps #[item1, item2]
  let result := layout col 200 300
  let tl1 := result.get! 1
  let tl2 := result.get! 2
  -- Main axis (height): content height
  shouldBeNear tl1.height 40 0.01
  shouldBeNear tl2.height 50 0.01
  -- Cross axis (width): content width (no stretch)
  shouldBeNear tl1.width 80 0.01
  shouldBeNear tl2.width 60 0.01

/-! ## Containers with Pre-set Content Sizes (Afferent Pattern)

When containers have pre-computed content sizes (as afferent does), children
must still have their sizes computed for layout to work correctly.
-/

test "content: container with preset content - children still sized" := do
  -- This mimics the afferent pattern where containers get pre-computed content sizes
  -- but children still need to be laid out correctly
  let tab1 := LayoutNode.leaf 1 (ContentSize.mk' 60 24)
  let tab2 := LayoutNode.leaf 2 (ContentSize.mk' 80 24)
  let tab3 := LayoutNode.leaf 3 (ContentSize.mk' 50 24)
  -- Create row with explicit content size (like afferent's measureWidget does)
  let presetContent := ContentSize.mk' 200 30  -- Pre-computed by parent
  let row := LayoutNode.mk 0 {} (.flex (FlexContainer.row 5)) .none (some presetContent) #[tab1, tab2, tab3]
  let result := layout row 500 100
  -- Children should be sized by their content, not zero
  let tl1 := result.get! 1
  let tl2 := result.get! 2
  let tl3 := result.get! 3
  shouldBeNear tl1.width 60 0.01
  shouldBeNear tl2.width 80 0.01
  shouldBeNear tl3.width 50 0.01

test "content: nested containers with preset content" := do
  -- Nested case: row containing columns, each with preset content
  let leaf1 := LayoutNode.leaf 11 (ContentSize.mk' 40 20)
  let leaf2 := LayoutNode.leaf 21 (ContentSize.mk' 50 25)
  -- Columns with preset content (like afferent button/card containers)
  let col1 := LayoutNode.mk 1 {} (.flex (FlexContainer.column 0)) .none (some (ContentSize.mk' 40 20)) #[leaf1]
  let col2 := LayoutNode.mk 2 {} (.flex (FlexContainer.column 0)) .none (some (ContentSize.mk' 50 25)) #[leaf2]
  -- Row with preset content
  let row := LayoutNode.mk 0 {} (.flex (FlexContainer.row 10)) .none (some (ContentSize.mk' 100 25)) #[col1, col2]
  let result := layout row 500 100
  -- Deep children should still get their content sizes
  let tl11 := result.get! 11
  let tl21 := result.get! 21
  shouldBeNear tl11.width 40 0.01
  shouldBeNear tl21.width 50 0.01

test "content: grid with preset content - children sized" := do
  -- Grid with preset content size, children should still be sized
  let cell1 := LayoutNode.leaf 1 (ContentSize.mk' 30 30)
  let cell2 := LayoutNode.leaf 2 (ContentSize.mk' 40 40)
  let cell3 := LayoutNode.leaf 3 (ContentSize.mk' 35 35)
  let cell4 := LayoutNode.leaf 4 (ContentSize.mk' 45 45)
  let presetContent := ContentSize.mk' 200 200
  let grid := LayoutNode.mk 0 {} (.grid (GridContainer.columns 2 5)) .none (some presetContent) #[cell1, cell2, cell3, cell4]
  let result := layout grid 500 500
  -- Children should use their content sizes in grid cells
  let tl1 := result.get! 1
  let tl2 := result.get! 2
  -- Note: Grid may stretch to fill cells, but positions should be based on content
  -- At minimum, ensure children are laid out and have non-zero dimensions
  shouldSatisfy (tl1.width > 0) "cell1 should have width"
  shouldSatisfy (tl2.width > 0) "cell2 should have width"



end TrellisTests.ContentSizeTests
