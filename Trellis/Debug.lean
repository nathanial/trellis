/-
  Trellis Debug Types
  Structured introspection data for layout computations.
-/
import Std.Data.HashMap
import Trellis.Types
import Trellis.Axis
import Trellis.Flex
import Trellis.Grid
import Trellis.Result

namespace Trellis

/-- Debug view of a flex item during layout. -/
structure FlexItemDebug where
  nodeId : Nat
  sourceIndex : Nat
  margin : EdgeInsets
  hypotheticalMainSize : Length
  flexBaseSize : Length
  minMainSize : Length
  maxMainSize : Length
  hypotheticalCrossSize : Length
  flexGrow : Float
  flexShrink : Float
  baseline : Length
  frozen : Bool
  resolvedMainSize : Length
  resolvedCrossSize : Length
deriving Repr, Inhabited

/-- Debug view of a flex line. -/
structure FlexLineDebug where
  items : Array FlexItemDebug
  usedMainSpace : Length
  crossSize : Length
  crossPosition : Length
  maxBaseline : Length
  mainPositions : Array Length := #[]
  crossPositions : Array Length := #[]
deriving Repr, Inhabited

/-- Debug view of a flex container layout. -/
structure FlexLayoutDebug where
  container : FlexContainer
  axis : AxisInfo
  availableMain : Length
  availableCross : Length
  items : Array FlexItemDebug := #[]
  sortedItems : Array FlexItemDebug := #[]
  lines : Array FlexLineDebug := #[]
deriving Repr, Inhabited

/-- Debug view of a grid item during layout. -/
structure GridItemDebug where
  nodeId : Nat
  margin : EdgeInsets
  rowStart : Nat
  rowEnd : Nat
  colStart : Nat
  colEnd : Nat
  contentWidth : Length
  contentHeight : Length
  baseline : Length
  resolvedX : Length
  resolvedY : Length
  resolvedWidth : Length
  resolvedHeight : Length
deriving Repr, Inhabited

/-- Debug view of a resolved grid track. -/
structure ResolvedTrackDebug where
  size : TrackSize
  baseSize : Length
  frValue : Float
  minSize : Option Length
  maxSize : Option Length
  resolvedSize : Length
  position : Length
deriving Repr, Inhabited

/-- Debug view of a grid container layout. -/
structure GridLayoutDebug where
  container : GridContainer
  availableWidth : Length
  availableHeight : Length
  rowGap : Length
  columnGap : Length
  expandedRowTracks : Array GridTrack := #[]
  expandedColTracks : Array GridTrack := #[]
  rowLineNames : Array (String × Array Nat) := #[]
  colLineNames : Array (String × Array Nat) := #[]
  explicitRows : Nat := 0
  explicitCols : Nat := 0
  actualRows : Nat := 0
  actualCols : Nat := 0
  useRowSubgrid : Bool := false
  useColSubgrid : Bool := false
  items : Array GridItemDebug := #[]
  placedItems : Array GridItemDebug := #[]
  positionedItems : Array GridItemDebug := #[]
  rowTracks : Array ResolvedTrackDebug := #[]
  colTracks : Array ResolvedTrackDebug := #[]
  rowBaselines : Array Length := #[]
deriving Repr, Inhabited

/-- Debug data collected across a full layout pass. -/
structure LayoutDebug where
  intrinsicSizes : Std.HashMap Nat (Length × Length) := {}
  flex : Std.HashMap Nat FlexLayoutDebug := {}
  grid : Std.HashMap Nat GridLayoutDebug := {}
deriving Inhabited

namespace LayoutDebug

def empty : LayoutDebug := {}

end LayoutDebug

/-- Result of a layout pass with debug information. -/
structure LayoutDebugResult where
  result : LayoutResult
  debug : LayoutDebug
deriving Inhabited

end Trellis
