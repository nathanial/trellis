/-
  Trellis Layout Result
  Output types from layout computation.
-/
import Trellis.Types
import Std.Data.HashMap

namespace Trellis

/-- A computed rectangle with position and size. -/
structure LayoutRect where
  x : Length
  y : Length
  width : Length
  height : Length
deriving Repr, BEq, Inhabited

namespace LayoutRect

def zero : LayoutRect := ⟨0, 0, 0, 0⟩

def mk' (x y width height : Length) : LayoutRect := ⟨x, y, width, height⟩

/-- Create from position and size. -/
def fromPosSize (x y : Length) (width height : Length) : LayoutRect :=
  ⟨x, y, width, height⟩

/-- Get the right edge x coordinate. -/
def right (r : LayoutRect) : Length := r.x + r.width

/-- Get the bottom edge y coordinate. -/
def bottom (r : LayoutRect) : Length := r.y + r.height

/-- Get the center point. -/
def center (r : LayoutRect) : Length × Length :=
  (r.x + r.width / 2, r.y + r.height / 2)

/-- Check if a point is inside this rect. -/
def contains (r : LayoutRect) (px py : Length) : Bool :=
  px >= r.x && px <= r.right && py >= r.y && py <= r.bottom

/-- Translate by an offset. -/
def translate (r : LayoutRect) (dx dy : Length) : LayoutRect :=
  ⟨r.x + dx, r.y + dy, r.width, r.height⟩

/-- Inset by edge amounts. -/
def inset (r : LayoutRect) (insets : EdgeInsets) : LayoutRect :=
  ⟨r.x + insets.left,
   r.y + insets.top,
   r.width - insets.horizontal,
   r.height - insets.vertical⟩

/-- Expand by edge amounts (opposite of inset). -/
def expand (r : LayoutRect) (insets : EdgeInsets) : LayoutRect :=
  ⟨r.x - insets.left,
   r.y - insets.top,
   r.width + insets.horizontal,
   r.height + insets.vertical⟩

end LayoutRect

/-- Computed layout for a single node. -/
structure ComputedLayout where
  nodeId : Nat
  /-- Border box (includes margin area for positioning). -/
  borderRect : LayoutRect
  /-- Content box (actual drawable area after padding). -/
  contentRect : LayoutRect
deriving Repr, BEq, Inhabited

namespace ComputedLayout

/-- Create a computed layout with same rect for border and content. -/
def simple (nodeId : Nat) (rect : LayoutRect) : ComputedLayout :=
  { nodeId, borderRect := rect, contentRect := rect }

/-- Create with insets applied. -/
def withPadding (nodeId : Nat) (rect : LayoutRect) (padding : EdgeInsets) : ComputedLayout :=
  { nodeId, borderRect := rect, contentRect := rect.inset padding }

/-- Get the main rect for drawing. -/
def rect (cl : ComputedLayout) : LayoutRect := cl.borderRect

/-- Get x position. -/
def x (cl : ComputedLayout) : Length := cl.borderRect.x

/-- Get y position. -/
def y (cl : ComputedLayout) : Length := cl.borderRect.y

/-- Get width. -/
def width (cl : ComputedLayout) : Length := cl.borderRect.width

/-- Get height. -/
def height (cl : ComputedLayout) : Length := cl.borderRect.height

end ComputedLayout

/-- Complete layout result for a tree.
    Uses both an array for iteration and a HashMap for O(1) lookups. -/
structure LayoutResult where
  layouts : Array ComputedLayout
  layoutMap : Std.HashMap Nat ComputedLayout := {}
deriving Inhabited

namespace LayoutResult

def empty : LayoutResult := ⟨#[], {}⟩

/-- Find layout by node ID. O(1) HashMap lookup. -/
def get (r : LayoutResult) (nodeId : Nat) : Option ComputedLayout :=
  r.layoutMap.get? nodeId

/-- Get layout, panicking if not found. -/
def get! (r : LayoutResult) (nodeId : Nat) : ComputedLayout :=
  match r.get nodeId with
  | some cl => cl
  | none => panic! s!"Layout not found for node {nodeId}"

/-- Add a computed layout. Maintains both array and HashMap. -/
def add (r : LayoutResult) (cl : ComputedLayout) : LayoutResult :=
  ⟨r.layouts.push cl, r.layoutMap.insert cl.nodeId cl⟩

/-- Merge with another result. Maintains both array and HashMap. -/
def merge (r1 r2 : LayoutResult) : LayoutResult :=
  let mergedMap := r2.layouts.foldl (init := r1.layoutMap) fun m cl =>
    m.insert cl.nodeId cl
  ⟨r1.layouts ++ r2.layouts, mergedMap⟩

/-- Get all rects for rendering. -/
def allRects (r : LayoutResult) : Array LayoutRect :=
  r.layouts.map (·.borderRect)

/-- Map over all layouts. Maintains both array and HashMap. -/
def map (r : LayoutResult) (f : ComputedLayout → ComputedLayout) : LayoutResult :=
  let newLayouts := r.layouts.map f
  let newMap := newLayouts.foldl (init := {}) fun m cl =>
    m.insert cl.nodeId cl
  ⟨newLayouts, newMap⟩

/-- Translate all layouts by an offset. -/
def translate (r : LayoutResult) (dx dy : Length) : LayoutResult :=
  r.map fun cl =>
    { cl with
      borderRect := cl.borderRect.translate dx dy
      contentRect := cl.contentRect.translate dx dy }

/-- Number of layouts. -/
def size (r : LayoutResult) : Nat := r.layouts.size

end LayoutResult

end Trellis
