/-
  Trellis Layout Algorithm
  CSS Flexbox and Grid layout computation.

  This module re-exports the layout algorithms from FlexAlgorithm and GridAlgorithm,
  and provides the main entry point for layout computation.
-/
import Trellis.Types
import Trellis.Flex
import Trellis.Grid
import Trellis.Node
import Trellis.Axis
import Trellis.Result
import Trellis.FlexAlgorithm
import Trellis.GridAlgorithm

namespace Trellis

/-! ## Shared Utilities -/

/-- Recursively measure the intrinsic size of a node from its content or children. -/
partial def measureIntrinsicSize (node : LayoutNode) : Length × Length :=
  match node.content with
  | some cs => (cs.width, cs.height)
  | none =>
    if node.isLeaf then (0, 0)
    else
      match node.container with
      | .flex props => measureFlexIntrinsic props node.children
      | .grid props => measureGridIntrinsic props node.children
      | .none => (0, 0)
where
  measureFlexIntrinsic (props : FlexContainer) (children : Array LayoutNode) : Length × Length :=
    if children.isEmpty then
      (0, 0)
    else
      let childSizes := children.map measureIntrinsicSize
      let gapCount := (children.size - 1).toFloat
      if props.direction.isHorizontal then
        let width := childSizes.foldl (fun acc sz => acc + sz.1) 0 + props.gap * gapCount
        let height := childSizes.foldl (fun acc sz => max acc sz.2) 0
        (width, height)
      else
        let width := childSizes.foldl (fun acc sz => max acc sz.1) 0
        let height := childSizes.foldl (fun acc sz => acc + sz.2) 0 + props.gap * gapCount
        (width, height)

  measureGridIntrinsic (props : GridContainer) (children : Array LayoutNode) : Length × Length := Id.run do
    if children.isEmpty then
      return (0, 0)

    let childSizes := children.map measureIntrinsicSize
    let areaRows := props.templateAreas.rowCount
    let areaCols := props.templateAreas.colCount
    let explicitRows :=
      if areaRows > 0 then areaRows
      else (getExpandedSizes props.templateRows 0 props.rowGap).size
    let explicitCols :=
      if areaCols > 0 then areaCols
      else (getExpandedSizes props.templateColumns 0 props.columnGap).size

    let childCount := children.size
    let ceilDiv := fun (n d : Nat) => if d == 0 then 0 else (n + d - 1) / d

    let (rowCount, colCount) := match props.autoFlow with
      | .row | .rowDense =>
        let colCount := if explicitCols > 0 then explicitCols
          else if explicitRows > 0 then max 1 (ceilDiv childCount explicitRows)
          else max 1 childCount
        let rowCount := if explicitRows > 0 then max explicitRows (ceilDiv childCount colCount)
          else max 1 (ceilDiv childCount colCount)
        (rowCount, colCount)
      | .column | .columnDense =>
        let rowCount := if explicitRows > 0 then explicitRows
          else if explicitCols > 0 then max 1 (ceilDiv childCount explicitCols)
          else max 1 childCount
        let colCount := if explicitCols > 0 then max explicitCols (ceilDiv childCount rowCount)
          else max 1 (ceilDiv childCount rowCount)
        (rowCount, colCount)

    let mut rowHeights : Array Length := (List.replicate rowCount 0).toArray
    let mut colWidths : Array Length := (List.replicate colCount 0).toArray

    for idx in [:childCount] do
      let size := childSizes[idx]!
      let (rowIdx, colIdx) := match props.autoFlow with
        | .row | .rowDense => (idx / colCount, idx % colCount)
        | .column | .columnDense => (idx % rowCount, idx / rowCount)
      if rowIdx < rowHeights.size then
        rowHeights := rowHeights.set! rowIdx (max rowHeights[rowIdx]! size.2)
      if colIdx < colWidths.size then
        colWidths := colWidths.set! colIdx (max colWidths[colIdx]! size.1)

    let rowGapCount := if rowCount > 0 then (rowCount - 1).toFloat else 0
    let colGapCount := if colCount > 0 then (colCount - 1).toFloat else 0
    let width := colWidths.foldl (· + ·) 0 + props.columnGap * colGapCount
    let height := rowHeights.foldl (· + ·) 0 + props.rowGap * rowGapCount
    return (width, height)

/-- Get the content size of a node. -/
def getContentSize (node : LayoutNode) : Length × Length :=
  measureIntrinsicSize node

/-! ## Main Layout Function -/

/-- Layout a single node and its children recursively. -/
partial def layoutNode (node : LayoutNode) (availableWidth availableHeight : Length)
    (offsetX offsetY : Length := 0) : LayoutResult := Id.run do
  let box := node.box

  -- Resolve node dimensions
  -- For containers with auto dimensions, use available space
  -- For leaf nodes, use content size
  let contentSize := getContentSize node
  let isContainer := !node.isLeaf
  let resolvedWidth := match box.width with
    | .auto => if isContainer then availableWidth else contentSize.1
    | dim => dim.resolve availableWidth contentSize.1
  let resolvedHeight := match box.height with
    | .auto => if isContainer then availableHeight else contentSize.2
    | dim => dim.resolve availableHeight contentSize.2
  -- Apply aspect-ratio if one dimension is auto
  let (resolvedWidth, resolvedHeight) := applyAspectRatio resolvedWidth resolvedHeight
    box.width.isAuto box.height.isAuto box.aspectRatio
  let width := box.clampWidth resolvedWidth
  let height := box.clampHeight resolvedHeight

  -- Create layout for this node
  let nodeRect := LayoutRect.mk' offsetX offsetY width height
  let mut result := LayoutResult.empty.add (ComputedLayout.withPadding node.id nodeRect box.padding)

  -- Layout children based on container type
  match node.container with
  | .flex props =>
    let childResult := layoutFlexContainer props node.children width height box.padding getContentSize
    -- Translate child results by node position
    let childResult := childResult.translate offsetX offsetY
    result := result.merge childResult

    -- Recursively layout any container children
    for child in node.children do
      if !child.isLeaf then
        if let some cl := childResult.get child.id then
          let grandchildResult := layoutNode child cl.borderRect.width cl.borderRect.height
                                  cl.borderRect.x cl.borderRect.y
          -- Only add grandchildren (child is already in result)
          for layout in grandchildResult.layouts do
            if layout.nodeId != child.id then
              result := result.add layout

  | .grid props =>
    let childResult := layoutGridContainer props node.children width height box.padding getContentSize
    let childResult := childResult.translate offsetX offsetY
    result := result.merge childResult

    -- Recursively layout any container children
    for child in node.children do
      if !child.isLeaf then
        if let some cl := childResult.get child.id then
          let grandchildResult := layoutNode child cl.borderRect.width cl.borderRect.height
                                  cl.borderRect.x cl.borderRect.y
          -- Only add grandchildren (child is already in result)
          for layout in grandchildResult.layouts do
            if layout.nodeId != child.id then
              result := result.add layout

  | .none =>
    -- Leaf node, no children to layout
    pure ()

  result

/-- Main entry point: Layout a tree starting from the root. -/
def layout (root : LayoutNode) (availableWidth availableHeight : Length) : LayoutResult :=
  layoutNode root availableWidth availableHeight

end Trellis
