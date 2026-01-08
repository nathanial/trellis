/-
  Trellis Grid Algorithm
  CSS Grid layout computation.
-/
import Trellis.Types
import Trellis.Grid
import Trellis.Flex  -- for AlignItems, AlignContent, JustifyContent
import Trellis.Node
import Trellis.Result

namespace Trellis

/-! ## Grid Item Intermediate State -/

/-- A resolved track (row or column) with computed size and position. -/
structure ResolvedTrack where
  size : TrackSize
  /-- Base size (from content or fixed). -/
  baseSize : Length := 0
  /-- Fr factor if flexible (from fr or minmax with fr max). -/
  frValue : Float := 0
  /-- Minimum size constraint (from minmax). -/
  minSize : Option Length := none
  /-- Maximum size constraint (from minmax). -/
  maxSize : Option Length := none
  /-- Final resolved size. -/
  resolvedSize : Length := 0
  /-- Position (cumulative offset from start). -/
  position : Length := 0
deriving Repr, Inhabited

/-- Intermediate state for a grid item during layout. -/
structure GridItemState where
  node : LayoutNode
  /-- Outer margin. -/
  margin : EdgeInsets
  /-- Resolved row start (0-indexed). -/
  rowStart : Nat
  /-- Resolved row end (exclusive, 0-indexed). -/
  rowEnd : Nat
  /-- Resolved column start (0-indexed). -/
  colStart : Nat
  /-- Resolved column end (exclusive, 0-indexed). -/
  colEnd : Nat
  /-- Content width of the item (including margins for track sizing). -/
  contentWidth : Length
  /-- Content height of the item (including margins for track sizing). -/
  contentHeight : Length
  /-- Distance from item top to baseline (for baseline alignment). -/
  baseline : Length := 0
  /-- Final X position. -/
  resolvedX : Length := 0
  /-- Final Y position. -/
  resolvedY : Length := 0
  /-- Final width. -/
  resolvedWidth : Length := 0
  /-- Final height. -/
  resolvedHeight : Length := 0
deriving Repr, Inhabited

/-- Occupancy grid for auto-placement. -/
structure OccupancyGrid where
  /-- 2D array of occupied cells (row-major). -/
  cells : Array (Array Bool)
  rows : Nat
  cols : Nat
deriving Repr, Inhabited

namespace OccupancyGrid

/-- Create an empty occupancy grid. -/
def create (rows cols : Nat) : OccupancyGrid :=
  { cells := (List.replicate rows (List.replicate cols false).toArray).toArray, rows, cols }

/-- Check if a cell is occupied. -/
def isOccupied (grid : OccupancyGrid) (row col : Nat) : Bool :=
  if h : row < grid.cells.size then
    let rowArr := grid.cells[row]
    if col < rowArr.size then rowArr[col]! else false
  else false

/-- Check if a range of cells is available. -/
def isAreaAvailable (grid : OccupancyGrid) (rowStart rowEnd colStart colEnd : Nat) : Bool :=
  (List.range (rowEnd - rowStart)).all fun dr =>
    (List.range (colEnd - colStart)).all fun dc =>
      !grid.isOccupied (rowStart + dr) (colStart + dc)

/-- Mark a range of cells as occupied. -/
def markOccupied (grid : OccupancyGrid) (rowStart rowEnd colStart colEnd : Nat) : OccupancyGrid := Id.run do
  let mut cells := grid.cells
  for r in [rowStart:rowEnd] do
    if h : r < cells.size then
      let mut row := cells[r]
      for c in [colStart:colEnd] do
        if c < row.size then
          row := row.set! c true
      cells := cells.set! r row
  { grid with cells }

/-- Extend grid to accommodate more rows. -/
def extendRows (grid : OccupancyGrid) (newRows : Nat) : OccupancyGrid :=
  if newRows <= grid.rows then grid
  else
    let additionalRows := newRows - grid.rows
    let newCells := grid.cells ++ (List.replicate additionalRows (List.replicate grid.cols false).toArray).toArray
    { grid with cells := newCells, rows := newRows }

/-- Extend grid to accommodate more columns. -/
def extendCols (grid : OccupancyGrid) (newCols : Nat) : OccupancyGrid :=
  if newCols <= grid.cols then grid
  else
    let additionalCols := newCols - grid.cols
    let newCells := grid.cells.map fun row =>
      row ++ (List.replicate additionalCols false).toArray
    { grid with cells := newCells, cols := newCols }

end OccupancyGrid

/-- Auto-placement cursor for sparse packing. -/
structure PlacementCursor where
  row : Nat := 0
  col : Nat := 0
deriving Repr, Inhabited

/-! ## Named Lines and Areas -/

/-- Mapping from line names to line indices (0-indexed). -/
abbrev LineNameMap := Array (String × Array Nat)

/-- A named grid area derived from template areas. -/
structure GridArea where
  name : String
  rowStart : Nat
  rowEnd : Nat
  colStart : Nat
  colEnd : Nat
deriving Repr, Inhabited

/-- Internal bounds for building named areas. -/
structure AreaBounds where
  name : String
  rowMin : Nat
  rowMax : Nat
  colMin : Nat
  colMax : Nat
deriving Repr, Inhabited

/-- Add a line name mapping. -/
def addLineName (names : LineNameMap) (name : String) (idx : Nat) : LineNameMap := Id.run do
  let mut result := names
  let mut found := false
  for i in [:names.size] do
    let (n, idxs) := names[i]!
    if n == name then
      found := true
      result := result.set! i (n, idxs.push idx)
  if !found then
    result := result.push (name, #[idx])
  result

/-- Find line indices for a given name. -/
def findLineName (names : LineNameMap) (name : String) : Option (Array Nat) := Id.run do
  for entry in names do
    if entry.fst == name then
      return some entry.snd
  none

/-- Resolve a name to a specific line index (first for start, last for end). -/
def resolveLineName (names : LineNameMap) (name : String) (isEnd : Bool) : Option Nat :=
  match findLineName names name with
  | some idxs =>
    if idxs.isEmpty then none
    else
      let idx := if isEnd then idxs[idxs.size - 1]! else idxs[0]!
      some idx
  | none => none

/-- Merge line name mappings. -/
def mergeLineNames (base extra : LineNameMap) : LineNameMap := Id.run do
  let mut result := base
  for entry in extra do
    let (name, idxs) := entry
    for idx in idxs do
      result := addLineName result name idx
  result

/-- Build line name mappings from expanded tracks. -/
def buildTrackLineNames (tracks : Array GridTrack) : LineNameMap := Id.run do
  let mut names : LineNameMap := #[]
  for i in [:tracks.size] do
    if let some name := tracks[i]!.name then
      names := addLineName names name i
  names

/-- Collect named areas from template areas. -/
def collectTemplateAreas (areas : GridTemplateAreas) : Array GridArea := Id.run do
  let mut bounds : Array AreaBounds := #[]
  for r in [:areas.rows.size] do
    let row := areas.rows[r]!
    for c in [:row.size] do
      match row[c]! with
      | some name =>
        let mut found := false
        for i in [:bounds.size] do
          let b := bounds[i]!
          if b.name == name then
            found := true
            let updated := {
              b with
                rowMin := min b.rowMin r,
                rowMax := max b.rowMax r,
                colMin := min b.colMin c,
                colMax := max b.colMax c
            }
            bounds := bounds.set! i updated
        if !found then
          bounds := bounds.push { name, rowMin := r, rowMax := r, colMin := c, colMax := c }
      | none => pure ()
  bounds.map fun b =>
    { name := b.name
      rowStart := b.rowMin
      rowEnd := b.rowMax + 1
      colStart := b.colMin
      colEnd := b.colMax + 1 }

/-- Build line names from named areas (adds "<area>-start" and "<area>-end"). -/
def buildAreaLineNames (areas : Array GridArea) : LineNameMap × LineNameMap := Id.run do
  let mut rowNames : LineNameMap := #[]
  let mut colNames : LineNameMap := #[]
  for area in areas do
    rowNames := addLineName rowNames (area.name ++ "-start") area.rowStart
    rowNames := addLineName rowNames (area.name ++ "-end") area.rowEnd
    colNames := addLineName colNames (area.name ++ "-start") area.colStart
    colNames := addLineName colNames (area.name ++ "-end") area.colEnd
  (rowNames, colNames)

/-- Find a named area by name. -/
def findArea (areas : Array GridArea) (name : String) : Option GridArea := Id.run do
  for area in areas do
    if area.name == name then
      return some area
  none

/-! ## Absolute Positioning -/

/-- Resolve an absolutely positioned child relative to the parent's content box. -/
def resolveAbsoluteRect (child : LayoutNode) (availableWidth availableHeight : Length)
    (padding : EdgeInsets) (getContentSize : LayoutNode → Length × Length) : LayoutRect :=
  let box := child.box
  let contentSize := getContentSize child
  let isContainer := !child.isLeaf
  let baseWidth := match box.width with
    | .auto =>
      match box.left, box.right with
      | some l, some r => max 0 (availableWidth - l - r)
      | _, _ => if isContainer then availableWidth else contentSize.1
    | dim => dim.resolve availableWidth contentSize.1
  let baseHeight := match box.height with
    | .auto =>
      match box.top, box.bottom with
      | some t, some b => max 0 (availableHeight - t - b)
      | _, _ => if isContainer then availableHeight else contentSize.2
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

def partitionAbsolute (children : Array LayoutNode) : Array LayoutNode × Array LayoutNode :=
  children.foldl (fun acc child =>
    let (flow, abs) := acc
    if child.box.position == .absolute then
      (flow, abs.push child)
    else
      (flow.push child, abs)
  ) (#[], #[])

/-! ## Grid Layout Functions -/

/-- Resolve a GridLine to a 0-indexed track index. -/
def resolveGridLine (line : GridLine) (trackCount : Nat) (lineNames : LineNameMap) (isEnd : Bool) : Nat :=
  match line with
  | .auto => if isEnd then 1 else 0  -- Will be determined by auto-placement
  | .line n =>
    if n >= 0 then
      min (n.toNat - 1) trackCount  -- CSS lines are 1-indexed
    else
      -- Negative lines count from end (e.g., -1 = last line)
      let fromEnd := (-n).toNat
      if fromEnd <= trackCount + 1 then trackCount + 1 - fromEnd else 0
  | .span n => if isEnd then n else 0
  | .named s =>
    match resolveLineName lineNames s isEnd with
    | some idx => min idx trackCount
    | none => 0

/-- Check if a GridSpan has explicit placement. -/
def hasExplicitPlacement (span : GridSpan) : Bool :=
  match span.start with
  | .auto => false
  | .line _ => true
  | .span _ => false  -- span alone is not explicit
  | .named _ => true

/-- Get the span count from a GridSpan. -/
def getSpanCount (span : GridSpan) : Nat :=
  match span.finish with
  | .span n => n
  | _ => 1

/-- Resolve a GridSpan to start and end indices. -/
def resolveGridSpan (span : GridSpan) (trackCount : Nat) (lineNames : LineNameMap := #[])
    (defaultStart : Nat := 0) : Nat × Nat :=
  let startIdx := match span.start with
    | .auto => defaultStart
    | .span _ => defaultStart
    | _ => resolveGridLine span.start trackCount lineNames false
  let endIdx := match span.finish with
    | .auto => startIdx + 1
    | .span n => startIdx + n
    | _ => resolveGridLine span.finish trackCount lineNames true
  (startIdx, max (startIdx + 1) endIdx)

/-- Extract fr value from a TrackSize (including nested minmax). -/
def extractFrValue : TrackSize → Float
  | .fr n => n
  | .minmax _ maxT => extractFrValue maxT
  | _ => 0

/-- Resolve a TrackSize to a fixed Length if possible. -/
def resolveToLength (size : TrackSize) (available : Length) : Option Length :=
  match size with
  | .fixed dim => some (dim.resolve available 0)
  | .fr _ => none  -- Fr cannot be resolved to fixed length
  | .minmax minTrack maxTrack =>
    -- For minmax, we can resolve if both parts resolve
    match resolveToLength minTrack available, resolveToLength maxTrack available with
    | some minLen, some maxLen => some (max minLen maxLen)
    | _, _ => none
  | .fitContent maxLen => some maxLen

/-- Calculate the minimum size of a track size (for auto-fill calculation). -/
def minTrackSize (size : TrackSize) (available : Length) : Length :=
  match size with
  | .fixed dim => dim.resolve available 0
  | .fr _ => 0  -- Fr tracks have no intrinsic minimum
  | .minmax minTrack _ => minTrackSize minTrack available
  | .fitContent _ => 0

/-- Calculate how many times a repeat pattern fits in available space (for auto-fill/auto-fit). -/
def calculateAutoRepeatCount (sizes : Array TrackSize) (available gap : Length) : Nat :=
  if sizes.isEmpty then 0
  else
    -- Calculate minimum size of one repeat cycle
    let cycleSize := sizes.foldl (fun acc s => acc + minTrackSize s available) 0
    let cycleWithGap := cycleSize + gap * (sizes.size - 1).toFloat
    if cycleWithGap <= 0 then 1  -- Avoid division by zero, at least 1 track
    else
      -- How many complete cycles fit?
      let count := (available / cycleWithGap).toUInt64.toNat
      max 1 count  -- At least 1 repeat

/-- Determine repeat count for a track entry. -/
def repeatEntryCount (mode : RepeatMode) (sizes : Array TrackSize) (available gap : Length) : Nat :=
  match mode with
  | .count n => n
  | .autoFill => calculateAutoRepeatCount sizes available gap
  | .autoFit => calculateAutoRepeatCount sizes available gap  -- Same as autoFill for now

/-- Expand track entries to a flat array of TrackSizes. -/
def expandEntries (entries : Array TrackEntry) (available gap : Length) : Array TrackSize := Id.run do
  let mut result : Array TrackSize := #[]
  for entry in entries do
    match entry with
    | .single track => result := result.push track.size
    | .repeat mode sizes =>
      let repeatCount := repeatEntryCount mode sizes available gap
      for _ in [:repeatCount] do
        for size in sizes do
          result := result.push size
  result

/-- Expand track entries to a flat array of GridTracks (preserving names when available). -/
def expandEntryTracks (entries : Array TrackEntry) (available gap : Length) : Array GridTrack := Id.run do
  let mut result : Array GridTrack := #[]
  for entry in entries do
    match entry with
    | .single track => result := result.push track
    | .repeat mode sizes =>
      let repeatCount := repeatEntryCount mode sizes available gap
      for _ in [:repeatCount] do
        for size in sizes do
          result := result.push { size, name := none }
  result

/-- Get expanded track sizes from a GridTemplate (handles both legacy tracks and entries). -/
def getExpandedSizes (template : GridTemplate) (available gap : Length) : Array TrackSize :=
  if template.entries.isEmpty then
    -- Use legacy tracks array
    template.tracks.map (·.size)
  else
    -- Expand entries
    expandEntries template.entries available gap

/-- Get expanded track definitions from a GridTemplate (preserving line names). -/
def getExpandedTracks (template : GridTemplate) (available gap : Length) : Array GridTrack :=
  if template.entries.isEmpty then
    template.tracks
  else
    expandEntryTracks template.entries available gap

/-- Initialize ResolvedTrack array from a GridTemplate. -/
def initTracks (template : GridTemplate) (minCount : Nat) (available : Length := 0) (gap : Length := 0) : Array ResolvedTrack := Id.run do
  let expandedSizes := getExpandedSizes template available gap
  let explicitCount := expandedSizes.size
  let count := max explicitCount minCount
  let mut tracks : Array ResolvedTrack := #[]
  for i in [:count] do
    let size := if i < explicitCount then expandedSizes[i]! else template.autoSize
    -- Extract fr value (including from minmax max)
    let frVal := extractFrValue size
    -- Extract min/max constraints from minmax
    let (minSz, maxSz) := match size with
      | .minmax minT maxT =>
        let minL := resolveToLength minT available
        let maxL := resolveToLength maxT available
        (minL, maxL)
      | .fitContent maxLen => (none, some maxLen)
      | _ => (none, none)
    tracks := tracks.push { size, frValue := frVal, minSize := minSz, maxSize := maxSz }
  tracks

/-- Resolve the base size of a track (for fixed/auto sizes). -/
def resolveTrackBaseSize (size : TrackSize) (available : Length) : Length :=
  match size with
  | .fixed dim => dim.resolve available 0
  | .fr _ => 0  -- Fr tracks start at 0, sized in fr resolution phase
  | .minmax minTrack _ => resolveTrackBaseSize minTrack available
  | .fitContent maxLen => min maxLen available

/-- Calculate the maximum content size for items in a given track (including margins). -/
def maxContentInTrack (items : Array GridItemState) (trackIdx : Nat) (isColumn : Bool) : Length :=
  items.foldl (fun acc item =>
    let inTrack := if isColumn then
      item.colStart <= trackIdx && trackIdx < item.colEnd
    else
      item.rowStart <= trackIdx && trackIdx < item.rowEnd
    if inTrack then
      -- For spanning items, divide content size by span count
      let span := if isColumn then item.colEnd - item.colStart else item.rowEnd - item.rowStart
      -- Include margins in content size
      let size := if isColumn then
        item.contentWidth + item.margin.horizontal
      else
        item.contentHeight + item.margin.vertical
      max acc (size / span.toFloat)
    else acc
  ) 0

/-- Size tracks based on content and fixed sizes. -/
def sizeTracksToContent (tracks : Array ResolvedTrack) (items : Array GridItemState)
    (isColumn : Bool) (available : Length) : Array ResolvedTrack := Id.run do
  let mut result := tracks
  for i in [:tracks.size] do
    let track := tracks[i]!
    let baseSize := match track.size with
      | .fixed .auto => maxContentInTrack items i isColumn
      | .fixed dim => dim.resolve available 0
      | .fr _ => 0  -- Fr tracks sized in fr resolution phase
      | .minmax minTrack maxTrack =>
        -- Base size starts at min
        let minSz := resolveTrackBaseSize minTrack available
        -- If max is not fr, we can size to content up to max
        if maxTrack.isFr then
          minSz  -- Will be grown in fr resolution phase
        else
          -- Grow based on content, clamped between min and max
          let content := maxContentInTrack items i isColumn
          let maxSz := resolveTrackBaseSize maxTrack available
          max minSz (min content maxSz)
      | .fitContent maxLen =>
        min (maxContentInTrack items i isColumn) maxLen
    result := result.set! i { track with baseSize, resolvedSize := baseSize }
  result

/-- Calculate total fr units in track array. -/
def totalFrUnits (tracks : Array ResolvedTrack) : Float :=
  tracks.foldl (fun acc t => acc + t.frValue) 0

/-- Distribute remaining space to fr tracks, respecting min/max constraints from minmax(). -/
def resolveFrTracks (tracks : Array ResolvedTrack) (available : Length) (gap : Length) : Array ResolvedTrack := Id.run do
  -- Calculate space used by non-fr tracks (including minmax base sizes)
  let usedSpace := tracks.foldl (fun acc t =>
    if t.frValue > 0 then acc + t.baseSize else acc + t.resolvedSize) 0
  let gaps := if tracks.size > 0 then gap * (tracks.size - 1).toFloat else 0
  let remaining := max 0 (available - usedSpace - gaps)

  let totalFr := totalFrUnits tracks
  if totalFr <= 0 then return tracks

  let spacePerFr := remaining / totalFr

  let mut result := tracks
  for i in [:tracks.size] do
    let track := tracks[i]!
    if track.frValue > 0 then
      -- Calculate raw fr size
      let frSize := spacePerFr * track.frValue
      -- For minmax tracks, the resolved size is base + fr growth, clamped to min
      -- For pure fr tracks, just use the fr size
      let resolvedSize := match track.minSize with
        | some minS => max minS (track.baseSize + frSize)
        | none => frSize
      result := result.set! i { track with resolvedSize }
  result

/-- Calculate cumulative positions for tracks. -/
def calculateTrackPositions (tracks : Array ResolvedTrack) (gap : Length) (startOffset : Length := 0) : Array ResolvedTrack := Id.run do
  let mut result := tracks
  let mut pos := startOffset
  for i in [:tracks.size] do
    let track := tracks[i]!
    result := result.set! i { track with position := pos }
    pos := pos + track.resolvedSize + gap
  result

/-- Get the bounds of a grid area from track positions. -/
def getGridArea (rowTracks colTracks : Array ResolvedTrack)
    (rowStart rowEnd colStart colEnd : Nat) (rowGap colGap : Length) : Length × Length × Length × Length :=
  let x := if colStart < colTracks.size then colTracks[colStart]!.position else 0
  let y := if rowStart < rowTracks.size then rowTracks[rowStart]!.position else 0

  -- Width spans from colStart to colEnd (exclusive)
  let width := Id.run do
    let mut w : Length := 0
    for c in [colStart:colEnd] do
      if c < colTracks.size then
        w := w + colTracks[c]!.resolvedSize
        if c + 1 < colEnd then w := w + colGap
    w

  -- Height spans from rowStart to rowEnd (exclusive)
  let height := Id.run do
    let mut h : Length := 0
    for r in [rowStart:rowEnd] do
      if r < rowTracks.size then
        h := h + rowTracks[r]!.resolvedSize
        if r + 1 < rowEnd then h := h + rowGap
    h

  (x, y, width, height)

/-- Compute the maximum baseline for each row in the grid.
    Only considers items that span exactly one row (baseline alignment
    is undefined for multi-row spanning items per CSS spec). -/
def computeRowBaselines (items : Array GridItemState) (rowCount : Nat)
    (alignItems : AlignItems) : Array Length := Id.run do
  let mut rowBaselines : Array Length := (List.replicate rowCount 0).toArray
  for item in items do
    -- Only single-row items participate in baseline alignment
    if item.rowEnd - item.rowStart == 1 then
      let rowIdx := item.rowStart
      if rowIdx < rowCount then
        -- Check if this item uses baseline alignment
        let gridItem := item.node.gridItem?.getD GridItem.default
        let alignSelf := gridItem.alignSelf.getD alignItems
        if alignSelf == .baseline then
          let baseline := item.margin.top + item.baseline
          rowBaselines := rowBaselines.set! rowIdx (max rowBaselines[rowIdx]! baseline)
  rowBaselines

/-- Apply alignment within a cell, accounting for margins and baseline.
    The rowBaseline parameter is the maximum baseline offset in this row,
    used for baseline alignment. -/
def alignInCell (itemWidth itemHeight : Length)
    (cellX cellY cellWidth cellHeight : Length)
    (margin : EdgeInsets) (itemBaseline rowBaseline : Length)
    (justifySelf alignSelf : AlignItems) : Length × Length × Length × Length :=
  -- Available space after margins
  let availWidth := cellWidth - margin.horizontal
  let availHeight := cellHeight - margin.vertical

  let (x, w) := match justifySelf with
    | .stretch => (cellX + margin.left, availWidth)
    | .flexStart => (cellX + margin.left, itemWidth)
    | .flexEnd => (cellX + cellWidth - margin.right - itemWidth, itemWidth)
    | .center => (cellX + margin.left + (availWidth - itemWidth) / 2, itemWidth)
    | .baseline => (cellX + margin.left, itemWidth)  -- Horizontal baseline = flexStart

  let (y, h) := match alignSelf with
    | .stretch => (cellY + margin.top, availHeight)
    | .flexStart => (cellY + margin.top, itemHeight)
    | .flexEnd => (cellY + cellHeight - margin.bottom - itemHeight, itemHeight)
    | .center => (cellY + margin.top + (availHeight - itemHeight) / 2, itemHeight)
    | .baseline =>
      -- Position so item's baseline aligns with row's baseline
      let offset := rowBaseline - itemBaseline
      (cellY + offset, itemHeight)

  (x, y, w, h)

/-- Auto-place an item in the grid. Returns updated item, occupancy grid, and cursor. -/
def autoPlaceItem (item : GridItemState) (occupancy : OccupancyGrid)
    (flow : GridAutoFlow) (cols rows : Nat) (cursor : PlacementCursor)
    : GridItemState × OccupancyGrid × PlacementCursor := Id.run do
  let rowSpan := item.rowEnd - item.rowStart
  let colSpan := item.colEnd - item.colStart

  -- Determine search parameters based on flow
  let isDense := match flow with
    | .rowDense | .columnDense => true
    | _ => false
  let isColumnFlow := match flow with
    | .column | .columnDense => true
    | _ => false

  -- Starting position: dense starts at (0,0), sparse starts at cursor
  let (startRow, startCol) := if isDense then (0, 0) else (cursor.row, cursor.col)

  let mut occ := occupancy
  let mut foundRow := 0
  let mut foundCol := 0
  let mut found := false
  let mut newCursorRow := cursor.row
  let mut newCursorCol := cursor.col

  if isColumnFlow then
    -- Column-major: outer loop columns, inner loop rows
    -- Column-flow creates implicit columns, not implicit rows
    let maxCol := occ.cols + 10  -- Allow implicit columns
    for c in [startCol:maxCol] do
      if found then break
      -- For sparse mode on cursor's column, start from cursor row; otherwise row 0
      let rowStart := if !isDense && c == startCol then startRow else 0
      for r in [rowStart:rows] do  -- Only search within explicit rows
        if found then break
        if r + rowSpan <= rows then  -- Must fit within explicit rows
          if c + colSpan > occ.cols then
            occ := occ.extendCols (c + colSpan)
          if r + rowSpan > occ.rows then
            occ := occ.extendRows (r + rowSpan)
          if occ.isAreaAvailable r (r + rowSpan) c (c + colSpan) then
            foundRow := r
            foundCol := c
            found := true
            -- Update cursor to next position (for sparse mode)
            if r + rowSpan < rows then
              newCursorRow := r + rowSpan
              newCursorCol := c
            else
              newCursorRow := 0
              newCursorCol := c + 1
  else
    -- Row-major: outer loop rows, inner loop columns (default behavior)
    let maxRow := occ.rows + 10
    for r in [startRow:maxRow] do
      if found then break
      let colStart := if !isDense && r == startRow then startCol else 0
      for c in [colStart:cols] do
        if found then break
        if c + colSpan <= cols then
          if r + rowSpan > occ.rows then
            occ := occ.extendRows (r + rowSpan)
          if occ.isAreaAvailable r (r + rowSpan) c (c + colSpan) then
            foundRow := r
            foundCol := c
            found := true
            -- Update cursor to next position
            if c + colSpan < cols then
              newCursorRow := r
              newCursorCol := c + colSpan
            else
              newCursorRow := r + 1
              newCursorCol := 0

  -- Mark area as occupied
  if foundRow + rowSpan > occ.rows then
    occ := occ.extendRows (foundRow + rowSpan)
  if foundCol + colSpan > occ.cols then
    occ := occ.extendCols (foundCol + colSpan)
  occ := occ.markOccupied foundRow (foundRow + rowSpan) foundCol (foundCol + colSpan)

  let newItem := { item with
    rowStart := foundRow
    rowEnd := foundRow + rowSpan
    colStart := foundCol
    colEnd := foundCol + colSpan
  }

  let newCursor : PlacementCursor := { row := newCursorRow, col := newCursorCol }
  (newItem, occ, newCursor)

/-- Place all grid items, resolving explicit positions and auto-placing others. -/
def placeAllItems (items : Array GridItemState) (container : GridContainer)
    (explicitRows explicitCols : Nat) (rowLineNames colLineNames : LineNameMap)
    (areas : Array GridArea) : Array GridItemState × Nat × Nat := Id.run do
  let cols := max 1 explicitCols
  let rows := max 1 explicitRows
  let mut placedItems : Array GridItemState := #[]
  let mut occupancy := OccupancyGrid.create rows cols
  let mut maxRow := rows
  let mut maxCol := cols

  -- First pass: place items with explicit positions
  for item in items do
    let gridItem := item.node.gridItem?.getD GridItem.default
    let placement := gridItem.placement
    let areaPlacement := gridItem.area.bind (findArea areas)
    let hasExplicitRow := match areaPlacement with
      | some _ => true
      | none => hasExplicitPlacement placement.row
    let hasExplicitCol := match areaPlacement with
      | some _ => true
      | none => hasExplicitPlacement placement.column

    if hasExplicitRow && hasExplicitCol then
      -- Both explicit - place directly
      let (rowStart, rowEnd, colStart, colEnd) := match areaPlacement with
        | some area => (area.rowStart, area.rowEnd, area.colStart, area.colEnd)
        | none =>
          let (rs, re) := resolveGridSpan placement.row maxRow rowLineNames
          let (cs, ce) := resolveGridSpan placement.column maxCol colLineNames
          (rs, re, cs, ce)
      maxRow := max maxRow rowEnd
      maxCol := max maxCol colEnd

      -- Extend occupancy if needed
      if maxRow > occupancy.rows then
        occupancy := occupancy.extendRows maxRow
      if maxCol > occupancy.cols then
        occupancy := occupancy.extendCols maxCol

      occupancy := occupancy.markOccupied rowStart rowEnd colStart colEnd
      placedItems := placedItems.push { item with rowStart, rowEnd, colStart, colEnd }
    else
      -- Will be auto-placed in second pass
      placedItems := placedItems.push item

  -- Second pass: auto-place remaining items
  let mut finalItems : Array GridItemState := #[]
  let mut cursor := PlacementCursor.mk 0 0  -- Initialize cursor for sparse placement
  for item in placedItems do
    let gridItem := item.node.gridItem?.getD GridItem.default
    let placement := gridItem.placement
    let areaPlacement := gridItem.area.bind (findArea areas)
    let hasExplicitRow := match areaPlacement with
      | some _ => true
      | none => hasExplicitPlacement placement.row
    let hasExplicitCol := match areaPlacement with
      | some _ => true
      | none => hasExplicitPlacement placement.column

    if hasExplicitRow && hasExplicitCol then
      -- Already placed
      finalItems := finalItems.push item
    else
      -- Auto-place
      let rowSpan := getSpanCount placement.row
      let colSpan := getSpanCount placement.column
      let itemWithSpan := { item with
        rowStart := 0, rowEnd := rowSpan,
        colStart := 0, colEnd := colSpan
      }
      let (placed, newOcc, newCursor) := autoPlaceItem itemWithSpan occupancy container.autoFlow maxCol maxRow cursor
      occupancy := newOcc
      cursor := newCursor
      maxRow := max maxRow placed.rowEnd
      maxCol := max maxCol placed.colEnd
      finalItems := finalItems.push placed

  (finalItems, maxRow, maxCol)

/-- Position all items in their grid cells. -/
def positionGridItems (items : Array GridItemState) (rowTracks colTracks : Array ResolvedTrack)
    (container : GridContainer) (rowBaselines : Array Length) : Array GridItemState := Id.run do
  let mut result : Array GridItemState := #[]

  for item in items do
    -- Get cell bounds
    let (cellX, cellY, cellWidth, cellHeight) := getGridArea rowTracks colTracks
      item.rowStart item.rowEnd item.colStart item.colEnd
      container.rowGap container.columnGap

    -- Get alignment
    let gridItem := item.node.gridItem?.getD GridItem.default
    let justifySelf := gridItem.justifySelf.getD container.justifyItems
    let alignSelf := gridItem.alignSelf.getD container.alignItems

    -- Resolve percentage dimensions against cell size
    let box := item.node.box
    let availWidth := cellWidth - item.margin.horizontal
    let availHeight := cellHeight - item.margin.vertical
    let resolvedItemWidth := box.width.resolve availWidth item.contentWidth
    let resolvedItemHeight := box.height.resolve availHeight item.contentHeight

    -- Get row baseline (for single-row items)
    let rowBaseline := if item.rowStart < rowBaselines.size then rowBaselines[item.rowStart]! else 0

    -- Apply alignment (with margins and baseline)
    let (x, y, w, h) := alignInCell resolvedItemWidth resolvedItemHeight
      cellX cellY cellWidth cellHeight item.margin item.baseline rowBaseline justifySelf alignSelf

    result := result.push { item with resolvedX := x, resolvedY := y, resolvedWidth := w, resolvedHeight := h }

  result

/-! ## Main Grid Layout Function -/

/-- Layout a grid container. -/
def layoutGridContainer (container : GridContainer) (children : Array LayoutNode)
    (containerWidth containerHeight : Length)
    (padding : EdgeInsets) (getContentSize : LayoutNode → Length × Length) : LayoutResult := Id.run do
  -- Phase 1: Available space
  let availableWidth := max 0 (containerWidth - padding.horizontal)
  let availableHeight := max 0 (containerHeight - padding.vertical)
  let (flowChildren, absChildren) := partitionAbsolute children

  -- Get explicit track counts (from expanded entries, legacy tracks, and template areas)
  let expandedColTracks := getExpandedTracks container.templateColumns availableWidth container.columnGap
  let expandedRowTracks := getExpandedTracks container.templateRows availableHeight container.rowGap
  let areaDefs := collectTemplateAreas container.templateAreas
  let areaRowCount := GridTemplateAreas.rowCount container.templateAreas
  let areaColCount := GridTemplateAreas.colCount container.templateAreas
  let explicitCols := max expandedColTracks.size areaColCount
  let explicitRows := max expandedRowTracks.size areaRowCount

  -- Build line name maps (track names + template area line names)
  let rowLineNames := buildTrackLineNames expandedRowTracks
  let colLineNames := buildTrackLineNames expandedColTracks
  let (areaRowLineNames, areaColLineNames) := buildAreaLineNames areaDefs
  let rowLineNames := mergeLineNames rowLineNames areaRowLineNames
  let colLineNames := mergeLineNames colLineNames areaColLineNames

  -- Initialize items with content sizes, margins, and baselines
  let mut items : Array GridItemState := #[]
  for child in flowChildren do
    let contentSize := getContentSize child
    let box := child.box
    let gridItem := child.gridItem?.getD GridItem.default
    let placement := gridItem.placement
    let areaPlacement := gridItem.area.bind (findArea areaDefs)
    let (rowStart, rowEnd, colStart, colEnd) := match areaPlacement with
      | some area => (area.rowStart, area.rowEnd, area.colStart, area.colEnd)
      | none =>
        let (rs, re) := resolveGridSpan placement.row explicitRows rowLineNames
        let (cs, ce) := resolveGridSpan placement.column explicitCols colLineNames
        (rs, re, cs, ce)
    -- Get baseline from content (distance from top to baseline)
    let baseline := match child.content with
      | some cs => cs.getBaseline
      | none => contentSize.2  -- Default: baseline at bottom
    items := items.push {
      node := child
      margin := box.margin
      rowStart, rowEnd, colStart, colEnd
      contentWidth := contentSize.1
      contentHeight := contentSize.2
      baseline
    }

  -- Phase 2: Place items (explicit + auto-placement)
  let minCols := max 1 explicitCols
  let (placedItems, actualRows, actualCols) :=
    placeAllItems items container explicitRows minCols rowLineNames colLineNames areaDefs

  -- Phase 3: Initialize and size tracks (pass gap for auto-fill/auto-fit calculation)
  let mut colTracks := initTracks container.templateColumns actualCols availableWidth container.columnGap
  let mut rowTracks := initTracks container.templateRows actualRows availableHeight container.rowGap

  -- Size tracks to content
  colTracks := sizeTracksToContent colTracks placedItems true availableWidth
  rowTracks := sizeTracksToContent rowTracks placedItems false availableHeight

  -- Phase 4: Resolve fr units
  colTracks := resolveFrTracks colTracks availableWidth container.columnGap
  rowTracks := resolveFrTracks rowTracks availableHeight container.rowGap

  -- Phase 5: Calculate positions
  colTracks := calculateTrackPositions colTracks container.columnGap padding.left
  rowTracks := calculateTrackPositions rowTracks container.rowGap padding.top

  -- Phase 5.5: Compute row baselines for baseline alignment
  let rowBaselines := computeRowBaselines placedItems rowTracks.size container.alignItems

  -- Phase 6: Position items
  let positionedItems := positionGridItems placedItems rowTracks colTracks container rowBaselines

  -- Build result
  let mut result := LayoutResult.empty
  for item in positionedItems do
    let rect := LayoutRect.mk' item.resolvedX item.resolvedY item.resolvedWidth item.resolvedHeight
    result := result.add (ComputedLayout.simple item.node.id rect)

  -- Absolute positioned children (do not affect grid flow)
  for child in absChildren do
    let rect := resolveAbsoluteRect child availableWidth availableHeight padding getContentSize
    result := result.add (ComputedLayout.simple child.id rect)

  result

end Trellis
