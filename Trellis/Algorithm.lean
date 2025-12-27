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

/-- Get the content size of a node. -/
def getContentSize (node : LayoutNode) : Length × Length :=
  match node.content with
  | some cs => (cs.width, cs.height)
  | none => (0, 0)  -- Containers measure children recursively

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
