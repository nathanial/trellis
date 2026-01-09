/-
  Trellis Flexbox Algorithm
  CSS Flexbox layout computation.
-/
import Trellis.Types
import Trellis.Flex
import Trellis.Node
import Trellis.Axis
import Trellis.Result

namespace Trellis

/-! ## Flex Item Intermediate State -/

/-- Intermediate state for a flex item during layout computation. -/
structure FlexItemState where
  node : LayoutNode
  /-- Original index in children array (for stable sort by order). -/
  sourceIndex : Nat
  /-- Outer margin. -/
  margin : EdgeInsets
  /-- Hypothetical main size (before flex grow/shrink). -/
  hypotheticalMainSize : Length
  /-- Base flex size (flex-basis resolved). -/
  flexBaseSize : Length
  /-- Min main size constraint. -/
  minMainSize : Length
  /-- Max main size constraint. -/
  maxMainSize : Length
  /-- Hypothetical cross size. -/
  hypotheticalCrossSize : Length
  /-- Flex grow factor. -/
  flexGrow : Float
  /-- Flex shrink factor. -/
  flexShrink : Float
  /-- Distance from cross-start to baseline (for baseline alignment). -/
  baseline : Length := 0
  /-- Is this item frozen (cannot flex further)? -/
  frozen : Bool := false
  /-- Resolved main size after flex. -/
  resolvedMainSize : Length := 0
  /-- Resolved cross size. -/
  resolvedCrossSize : Length := 0
deriving Repr, Inhabited

/-- A flex line (group of items on one line). -/
structure FlexLine where
  items : Array FlexItemState
  /-- Total main space used by items and gaps. -/
  usedMainSpace : Length
  /-- Cross size of this line. -/
  crossSize : Length
  /-- Position along cross axis. -/
  crossPosition : Length := 0
  /-- Maximum baseline offset in this line (for baseline alignment). -/
  maxBaseline : Length := 0
deriving Repr, Inhabited

namespace FlexLine

def itemCount (line : FlexLine) : Nat := line.items.size

def totalFlexGrow (line : FlexLine) : Float :=
  line.items.foldl (fun acc item => acc + item.flexGrow) 0

def totalFlexShrinkScaled (line : FlexLine) : Float :=
  line.items.foldl (fun acc item => acc + item.flexShrink * item.flexBaseSize) 0

def unfrozenCount (line : FlexLine) : Nat :=
  line.items.foldl (fun acc item => if item.frozen then acc else acc + 1) 0

end FlexLine

/-! ## Margin Collapse -/

/-- Calculate collapsed margin between two adjacent margins per CSS rules.
    - Both positive: max(m1, m2)
    - Both negative: min(m1, m2) (more negative wins)
    - Mixed: m1 + m2 (algebraic sum) -/
def collapseMargins (margin1 margin2 : Length) : Length :=
  if margin1 >= 0 && margin2 >= 0 then
    max margin1 margin2
  else if margin1 < 0 && margin2 < 0 then
    min margin1 margin2
  else
    margin1 + margin2

/-! ## Phase 2: Collect Flex Items -/

/-- Resolve flex-basis for an item. -/
def resolveFlexBasis (basis : Dimension) (contentMain : Length)
    (availableMain : Length) : Length :=
  match basis with
  | .auto => contentMain
  | .length l => l
  | .percent p => availableMain * p
  | .minContent | .maxContent => contentMain

/-- Collect flex items from children with initial measurements. -/
def collectFlexItems (axis : AxisInfo) (children : Array LayoutNode)
    (availableMain availableCross : Length) (getContentSize : LayoutNode → Length × Length) : Array FlexItemState := Id.run do
  let mut items : Array FlexItemState := #[]
  for idx in [:children.size] do
    let child := children[idx]!
    let flexProps := child.flexItem?.getD FlexItem.default
    let contentSize := getContentSize child
    let contentMain := axis.mainFromPair contentSize
    let contentCross := axis.crossFromPair contentSize

    -- Get constraints
    let box := child.box
    let minMain := axis.mainMin box
    let maxMain := (axis.mainMax box).getD Length.unbounded
    let minCross := axis.crossMin box
    let maxCross := (axis.crossMax box).getD Length.unbounded

    -- Resolve main dimension: use box.width/height if specified, otherwise flex-basis
    let mainDim := axis.mainDimension box
    let resolvedMain := match mainDim with
      | .auto => resolveFlexBasis flexProps.basis contentMain availableMain
      | _ => mainDim.resolve availableMain contentMain

    -- Resolve cross dimension if specified (supports percentage)
    let crossDim := axis.crossDimension box
    let resolvedCross := crossDim.resolve availableCross contentCross

    -- Compute hypothetical sizes (clamped to constraints)
    let hypotheticalMain := min maxMain (max minMain resolvedMain)
    let hypotheticalCross := min maxCross (max minCross resolvedCross)

    -- Get baseline from content (distance from cross-start to baseline)
    -- For row direction: baseline is vertical distance from top
    -- For column direction: baseline alignment typically uses cross-start
    let baseline := match child.content with
      | some cs => cs.getBaseline
      | none => hypotheticalCross  -- Default: baseline at bottom

    items := items.push {
      node := child
      sourceIndex := idx
      margin := box.margin
      hypotheticalMainSize := hypotheticalMain
      flexBaseSize := resolvedMain
      minMainSize := minMain
      maxMainSize := maxMain
      hypotheticalCrossSize := hypotheticalCross
      flexGrow := flexProps.grow
      flexShrink := flexProps.shrink
      baseline := baseline
    }
  return items

/-! ## Phase 3: Partition Into Lines -/

/-- Compute total main space used by items in a line. -/
def computeLineMainSpace (items : Array FlexItemState) (gap : Length) : Length :=
  if items.isEmpty then 0
  else
    let itemSizes := items.foldl (fun acc item =>
      acc + item.hypotheticalMainSize + item.margin.horizontal) 0
    let gaps := gap * (items.size - 1).toFloat
    itemSizes + gaps

/-- Compute cross size of a line (max of item cross sizes). -/
def computeLineCrossSize (items : Array FlexItemState) : Length :=
  items.foldl (fun acc item =>
    max acc (item.hypotheticalCrossSize + item.margin.vertical)) 0

/-- Compute maximum baseline offset in a line. -/
def computeLineMaxBaseline (items : Array FlexItemState) : Length :=
  items.foldl (fun acc item => max acc (item.margin.top + item.baseline)) 0

/-- Compute cross size when baseline alignment is used.
    Cross size = max(baseline) + max(height - baseline) + margins. -/
def computeLineCrossSizeWithBaseline (items : Array FlexItemState) : Length × Length :=
  let maxBaseline := computeLineMaxBaseline items
  -- Calculate the maximum distance below baseline
  let maxBelowBaseline := items.foldl (fun acc item =>
    let belowBaseline := item.hypotheticalCrossSize - item.baseline + item.margin.bottom
    max acc belowBaseline) 0
  let crossSize := maxBaseline + maxBelowBaseline
  (crossSize, maxBaseline)

/-- Partition items into flex lines based on wrapping. -/
def partitionIntoLines (items : Array FlexItemState) (wrapMode : FlexWrap)
    (availableMain gap : Length) : Array FlexLine := Id.run do
  if items.isEmpty then return #[]

  match wrapMode with
  | .nowrap =>
    -- Single line with all items
    let usedSpace := computeLineMainSpace items gap
    let (crossSize, maxBaseline) := computeLineCrossSizeWithBaseline items
    #[{ items, usedMainSpace := usedSpace, crossSize, maxBaseline }]
  | .wrap | .wrapReverse =>
    let mut lines : Array FlexLine := #[]
    let mut currentItems : Array FlexItemState := #[]
    let mut currentUsed : Length := 0

    for item in items do
      let itemSize := item.hypotheticalMainSize + item.margin.horizontal
      let wouldUse := currentUsed + itemSize +
        (if currentItems.isEmpty then 0 else gap)

      if !currentItems.isEmpty && wouldUse > availableMain then
        -- Start new line
        let (crossSize, maxBaseline) := computeLineCrossSizeWithBaseline currentItems
        lines := lines.push {
          items := currentItems
          usedMainSpace := currentUsed
          crossSize
          maxBaseline
        }
        currentItems := #[item]
        currentUsed := itemSize
      else
        currentItems := currentItems.push item
        currentUsed := wouldUse

    -- Add last line
    if !currentItems.isEmpty then
      let (crossSize, maxBaseline) := computeLineCrossSizeWithBaseline currentItems
      lines := lines.push {
        items := currentItems
        usedMainSpace := currentUsed
        crossSize
        maxBaseline
      }

    -- Note: wrap-reverse cross-axis direction is handled in alignFlexLines
    lines

/-! ## Phase 4: Resolve Flexible Lengths -/

/-- Check if all items in a line are frozen. -/
def allFrozen (items : Array FlexItemState) : Bool :=
  items.all (·.frozen)

/-- Calculate total flex grow of unfrozen items. -/
def unfrozenFlexGrow (items : Array FlexItemState) : Float :=
  items.foldl (fun acc item => if item.frozen then acc else acc + item.flexGrow) 0

/-- Calculate total scaled flex shrink of unfrozen items. -/
def unfrozenFlexShrinkScaled (items : Array FlexItemState) : Float :=
  items.foldl (fun acc item =>
    if item.frozen then acc else acc + item.flexShrink * item.flexBaseSize) 0

/-- Calculate remaining free space for unfrozen items. -/
def calculateFreeSpace (items : Array FlexItemState) (availableMain gap : Length) : Length :=
  let gaps := gap * (items.size - 1).toFloat
  let usedSpace := items.foldl (fun acc item =>
    if item.frozen then
      acc + item.resolvedMainSize + item.margin.horizontal
    else
      acc + item.hypotheticalMainSize + item.margin.horizontal) 0
  availableMain - usedSpace - gaps

/-- Distribute positive free space (grow) with iterative constraint resolution.
    Per CSS Flexbox spec: distribute space, freeze items that violate constraints,
    and repeat until all items are frozen or no violations occur. -/
def distributeGrowth (line : FlexLine) (availableMain gap : Length) : FlexLine := Id.run do
  -- Initialize: set all items to their hypothetical sizes, mark as unfrozen
  let mut items := line.items.map fun item =>
    { item with
      resolvedMainSize := item.hypotheticalMainSize
      frozen := item.flexGrow <= 0  -- Freeze items that can't grow
    }

  -- Iterative constraint resolution (max 100 iterations as safety)
  for _ in [:100] do
    if allFrozen items then break

    let freeSpace := calculateFreeSpace items availableMain gap
    if freeSpace <= 0 then break

    let totalGrow := unfrozenFlexGrow items
    if totalGrow <= 0 then break

    let spacePerGrow := freeSpace / totalGrow
    let mut anyFrozen := false
    let mut newItems : Array FlexItemState := #[]

    -- Distribute space to unfrozen items
    for item in items do
      if item.frozen then
        newItems := newItems.push item
      else
        let growth := spacePerGrow * item.flexGrow
        let newSize := item.hypotheticalMainSize + growth
        -- Check for constraint violation
        if newSize > item.maxMainSize then
          anyFrozen := true
          newItems := newItems.push { item with resolvedMainSize := item.maxMainSize, frozen := true }
        else if newSize < item.minMainSize then
          -- Shouldn't happen in grow, but handle it
          anyFrozen := true
          newItems := newItems.push { item with resolvedMainSize := item.minMainSize, frozen := true }
        else
          newItems := newItems.push { item with resolvedMainSize := newSize }

    items := newItems

    -- If no items were frozen this iteration, we're done
    if !anyFrozen then
      -- Freeze all remaining items
      items := items.map fun item => { item with frozen := true }
      break

  let newUsed := items.foldl (fun acc item =>
    acc + item.resolvedMainSize + item.margin.horizontal) 0
  let gaps := gap * (items.size - 1).toFloat
  { line with items, usedMainSpace := newUsed + gaps }

/-- Distribute negative free space (shrink) with iterative constraint resolution.
    Per CSS Flexbox spec: distribute shrinkage proportionally to (shrink × base size),
    freeze items that hit min constraint, and repeat until done. -/
def distributeShrinkage (line : FlexLine) (availableMain gap : Length) : FlexLine := Id.run do
  -- Initialize: set all items to their hypothetical sizes, mark as unfrozen
  let mut items := line.items.map fun item =>
    { item with
      resolvedMainSize := item.hypotheticalMainSize
      frozen := item.flexShrink <= 0 || item.flexBaseSize <= 0  -- Freeze items that can't shrink
    }

  -- Iterative constraint resolution (max 100 iterations as safety)
  for _ in [:100] do
    if allFrozen items then break

    let freeSpace := calculateFreeSpace items availableMain gap
    if freeSpace >= 0 then break  -- No overflow

    let overflow := -freeSpace
    let totalShrinkScaled := unfrozenFlexShrinkScaled items
    if totalShrinkScaled <= 0 then break

    let mut anyFrozen := false
    let mut newItems : Array FlexItemState := #[]

    -- Distribute shrinkage to unfrozen items
    for item in items do
      if item.frozen then
        newItems := newItems.push item
      else
        let shrinkRatio := (item.flexShrink * item.flexBaseSize) / totalShrinkScaled
        let shrinkage := overflow * shrinkRatio
        let newSize := item.hypotheticalMainSize - shrinkage
        -- Check for min constraint violation
        if newSize < item.minMainSize then
          anyFrozen := true
          newItems := newItems.push { item with resolvedMainSize := item.minMainSize, frozen := true }
        else
          newItems := newItems.push { item with resolvedMainSize := newSize }

    items := newItems

    -- If no items were frozen this iteration, we're done
    if !anyFrozen then
      -- Freeze all remaining items
      items := items.map fun item => { item with frozen := true }
      break

  let newUsed := items.foldl (fun acc item =>
    acc + item.resolvedMainSize + item.margin.horizontal) 0
  let gaps := gap * (items.size - 1).toFloat
  { line with items, usedMainSpace := newUsed + gaps }

/-- Resolve flexible lengths for a line. -/
def resolveFlexibleLengths (line : FlexLine) (availableMain gap : Length) : FlexLine :=
  let freeSpace := availableMain - line.usedMainSpace
  if freeSpace >= 0 then
    distributeGrowth line availableMain gap
  else
    distributeShrinkage line availableMain gap

/-! ## Phase 5: Cross Axis Sizing -/

/-- Resolve cross sizes based on align-items. -/
def resolveCrossSizes (line : FlexLine) (alignItems : AlignItems) : FlexLine :=
  let items := line.items.map fun item =>
    let alignSelf := match item.node.flexItem? with
      | some fi => fi.alignSelf.getD alignItems
      | none => alignItems
    let crossSize := match alignSelf with
      | .stretch => line.crossSize - item.margin.vertical
      | _ => item.hypotheticalCrossSize
    { item with resolvedCrossSize := crossSize }
  { line with items }

/-! ## Phase 6: Main Axis Alignment (justify-content) -/

/-- Compute main axis positions for items in a line.
    When marginCollapse is true and axis is vertical (column), adjacent vertical
    margins are collapsed according to CSS rules. -/
def computeMainPositions (items : Array FlexItemState)
    (justify : JustifyContent) (availableMain gap : Length)
    (isReversed : Bool) (axis : AxisInfo) (marginCollapse : Bool) : Array Length := Id.run do
  let n := items.size
  if n == 0 then return #[]

  -- Compute effective margins (with optional collapsing for column direction)
  let shouldCollapse := marginCollapse && !axis.isHorizontal
  let effectiveMargins : Array (Length × Length) := Id.run do
    let mut result : Array (Length × Length) := #[]
    for i in [:n] do
      let item := items[i]!
      let startMargin := axis.mainStart item.margin
      let endMargin := axis.mainEnd item.margin
      if shouldCollapse && i > 0 then
        -- Collapse this item's start margin with previous item's end margin
        let prevEnd := axis.mainEnd items[i - 1]!.margin
        let collapsed := collapseMargins prevEnd startMargin
        -- Previous item gets 0 end margin, this item gets the collapsed margin
        result := result.push (collapsed, endMargin)
      else
        result := result.push (startMargin, endMargin)
    -- When collapsing, set end margins to 0 for all but last item
    if shouldCollapse && n > 1 then
      result := result.mapIdx fun i (start, end_) =>
        if i < n - 1 then (start, 0) else (start, end_)
    result

  let totalItemSize := items.foldl (fun acc i => acc + i.resolvedMainSize) 0
  let totalMargins := effectiveMargins.foldl (fun acc (start, end_) => acc + start + end_) 0
  let totalGaps := gap * (n - 1).toFloat
  let usedSpace := totalItemSize + totalMargins + totalGaps
  let freeSpace := availableMain - usedSpace

  let (startOffset, itemGap) := match justify with
    | .flexStart => (0.0, gap)
    | .flexEnd => (freeSpace, gap)
    | .center => (freeSpace / 2.0, gap)
    | .spaceBetween =>
        if n == 1 then (0.0, 0.0)
        else (0.0, freeSpace / (n - 1).toFloat + gap)
    | .spaceAround =>
        let space := freeSpace / n.toFloat
        (space / 2.0, space + gap)
    | .spaceEvenly =>
        let space := freeSpace / (n + 1).toFloat
        (space, space + gap)

  let mut positions : Array Length := #[]
  let mut currentPos := startOffset

  for i in [:n] do
    let item := items[i]!
    let (startMargin, endMargin) := effectiveMargins[i]!
    positions := positions.push (currentPos + startMargin)
    currentPos := currentPos + startMargin + endMargin + item.resolvedMainSize + itemGap

  if isReversed then
    -- Reverse positions relative to available space
    positions.mapIdx fun i pos =>
      availableMain - pos - items[i]!.resolvedMainSize
  else
    positions

/-! ## Phase 7: Cross Axis Alignment (align-items) -/

/-- Compute cross axis positions for items in a line.
    For baseline alignment, items are positioned so their baselines align
    at the line's maxBaseline offset. -/
def computeCrossPositions (items : Array FlexItemState)
    (alignItems : AlignItems) (lineCrossSize maxBaseline : Length) : Array Length :=
  items.map fun item =>
    let alignSelf := match item.node.flexItem? with
      | some fi => fi.alignSelf.getD alignItems
      | none => alignItems
    let itemCrossSize := item.resolvedCrossSize + item.margin.vertical
    match alignSelf with
    | .flexStart => item.margin.top
    | .baseline =>
      -- Position so item's baseline aligns with line's maxBaseline
      -- Item cross position = maxBaseline - item.baseline
      -- (accounting for margin.top which is already part of maxBaseline calculation)
      maxBaseline - item.baseline
    | .flexEnd => lineCrossSize - item.margin.bottom - item.resolvedCrossSize
    | .center => (lineCrossSize - itemCrossSize) / 2.0 + item.margin.top
    | .stretch => item.margin.top

/-! ## Phase 8: Align Content (multi-line) -/

/-- Position flex lines along the cross axis.
    For wrap-reverse, lines are positioned from cross-end toward cross-start. -/
def alignFlexLines (lines : Array FlexLine) (alignContent : AlignContent)
    (availableCross rowGap : Length) (isWrapReverse : Bool := false) : Array FlexLine := Id.run do
  if lines.isEmpty then return #[]

  let totalLineCross := lines.foldl (fun acc l => acc + l.crossSize) 0
  let totalGaps := rowGap * (lines.size - 1).toFloat
  let freeSpace := availableCross - totalLineCross - totalGaps

  -- Note: wrap-reverse doesn't change the meaning of alignment keywords;
  -- the positioning logic below handles the reversed direction directly.
  let (startOffset, lineGap) := match alignContent with
    | .flexStart | .stretch => (0.0, rowGap)
    | .flexEnd => (freeSpace, rowGap)
    | .center => (freeSpace / 2.0, rowGap)
    | .spaceBetween =>
        if lines.size == 1 then (0.0, 0.0)
        else (0.0, freeSpace / (lines.size - 1).toFloat + rowGap)
    | .spaceAround =>
        let space := freeSpace / lines.size.toFloat
        (space / 2.0, space + rowGap)
    | .spaceEvenly =>
        let space := freeSpace / (lines.size + 1).toFloat
        (space, space + rowGap)

  let mut positioned : Array FlexLine := #[]

  if isWrapReverse then
    -- Position from cross-end toward cross-start
    -- First line goes at the bottom/right, subsequent lines stack upward/leftward
    let mut currentCross := availableCross - startOffset
    for line in lines do
      currentCross := currentCross - line.crossSize
      positioned := positioned.push { line with crossPosition := currentCross }
      currentCross := currentCross - lineGap
  else
    -- Normal positioning from cross-start toward cross-end
    let mut currentCross := startOffset
    for line in lines do
      positioned := positioned.push { line with crossPosition := currentCross }
      currentCross := currentCross + line.crossSize + lineGap

  positioned

/-! ## Absolute Positioning -/

/-- Resolve an absolutely positioned child relative to the parent's content box. -/
def resolveAbsoluteRectFlex (child : LayoutNode) (availableWidth availableHeight : Length)
    (padding : EdgeInsets) (getContentSize : LayoutNode → Length × Length) : LayoutRect :=
  let box := child.box
  let contentSize := getContentSize child
  let baseWidth := match box.width with
    | .auto =>
      match box.left, box.right with
      | some l, some r => max 0 (availableWidth - l - r)
      | _, _ => contentSize.1
    | dim => dim.resolve availableWidth contentSize.1
  let baseHeight := match box.height with
    | .auto =>
      match box.top, box.bottom with
      | some t, some b => max 0 (availableHeight - t - b)
      | _, _ => contentSize.2
    | dim => dim.resolve availableHeight contentSize.2
  let width := box.clampWidth baseWidth
  let height := box.clampHeight baseHeight
  let x := match box.left, box.right with
    | some l, _ => l
    | none, some r => availableWidth - r - width
    | none, none => 0
  let y := match box.top, box.bottom with
    | some t, _ => t
    | none, some b => availableHeight - b - height
    | none, none => 0
  let x := x + padding.left + box.margin.left
  let y := y + padding.top + box.margin.top
  LayoutRect.mk' x y width height

def partitionAbsoluteFlex (children : Array LayoutNode) : Array LayoutNode × Array LayoutNode :=
  children.foldl (fun acc child =>
    let (flow, abs) := acc
    if child.box.position == .absolute then
      (flow, abs.push child)
    else
      (flow.push child, abs)
  ) (#[], #[])

/-! ## Main Flex Layout Function -/

/-- Layout a flex container. -/
def layoutFlexContainer (container : FlexContainer) (children : Array LayoutNode)
    (containerWidth containerHeight : Length)
    (padding : EdgeInsets) (getContentSize : LayoutNode → Length × Length) : LayoutResult := Id.run do
  let axis := AxisInfo.fromDirection container.direction

  -- Phase 1: Available space
  let (availableWidth, availableHeight) :=
    (max 0 (containerWidth - padding.horizontal),
     max 0 (containerHeight - padding.vertical))
  let (availableMain, availableCross) :=
    (axis.mainSize availableWidth availableHeight,
     axis.crossSize availableWidth availableHeight)

  -- Phase 2: Collect items
  let (flowChildren, absChildren) := partitionAbsoluteFlex children
  let items := collectFlexItems axis flowChildren availableMain availableCross getContentSize

  -- Sort items by order (stable: items with same order keep source order)
  let items := items.qsort fun a b =>
    let orderA := a.node.flexItem?.map (·.order) |>.getD 0
    let orderB := b.node.flexItem?.map (·.order) |>.getD 0
    if orderA != orderB then orderA < orderB
    else a.sourceIndex < b.sourceIndex

  -- Phase 3: Partition into lines
  let lines := partitionIntoLines items container.wrap availableMain container.gap

  -- Phase 4: Resolve flexible lengths
  let lines := lines.map fun line =>
    resolveFlexibleLengths line availableMain container.gap

  -- Phase 8: Align content (position lines)
  let isWrapReverse := container.wrap == .wrapReverse
  -- For single-line containers with stretch, use full cross space
  let lines := if lines.size == 1 && container.alignContent == .stretch then
    lines.map fun line => { line with crossSize := availableCross }
  else
    alignFlexLines lines container.alignContent availableCross container.rowGap isWrapReverse

  -- Phase 5: Cross axis sizing (after line sizes are finalized)
  let lines := lines.map fun line =>
    resolveCrossSizes line container.alignItems

  -- Phases 6-7: Position items within lines
  let mut result := LayoutResult.empty

  for line in lines do
    -- Phase 6: Main axis positions
    let mainPositions := computeMainPositions line.items
                         container.justifyContent availableMain
                         container.gap axis.isReversed axis container.marginCollapse

    -- Phase 7: Cross axis positions
    let crossPositions := computeCrossPositions line.items
                          container.alignItems line.crossSize line.maxBaseline

    -- Build computed layouts
    for i in [:line.items.size] do
      if h : i < line.items.size then
        let item := line.items[i]
        let mainPos := mainPositions[i]! + axis.mainStart padding
        let crossPos := crossPositions[i]! + line.crossPosition + axis.crossStart padding

        let (x, y) := axis.toXY mainPos crossPos
        let (width, height) := axis.toWidthHeight item.resolvedMainSize item.resolvedCrossSize

        let rect := LayoutRect.mk' x y width height
        result := result.add (ComputedLayout.simple item.node.id rect)

  -- Absolute positioned children (do not affect layout flow)
  for child in absChildren do
    let rect := resolveAbsoluteRectFlex child availableWidth availableHeight padding getContentSize
    result := result.add (ComputedLayout.simple child.id rect)

  result

end Trellis
