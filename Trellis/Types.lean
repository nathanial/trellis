/-
  Trellis Layout Types
  Core dimension and box model types for CSS-style layout.
-/

namespace Trellis

/-- Length in layout units (pixels). -/
abbrev Length := Float

namespace Length

/-- A large value representing an unbounded maximum constraint.
    Used when no explicit max-width/max-height is set. -/
def unbounded : Length := 1000000.0

end Length

/-- A CSS-style dimension value. -/
inductive Dimension where
  | auto                  -- Size determined by content or context
  | length (l : Length)   -- Explicit length in pixels
  | percent (p : Float)   -- Percentage of parent (0.0 to 1.0)
  | minContent            -- Minimum content size
  | maxContent            -- Maximum content size
deriving Repr, BEq, Inhabited

namespace Dimension

def zero : Dimension := .length 0

def isAuto : Dimension → Bool
  | .auto => true
  | _ => false

/-- Resolve a dimension to a concrete length given available space and content size. -/
def resolve (d : Dimension) (available : Length) (content : Length) : Length :=
  match d with
  | .auto => content
  | .length l => l
  | .percent p => available * p
  | .minContent => content
  | .maxContent => content

end Dimension

/-- Edge insets for margin, padding, etc. -/
structure EdgeInsets where
  top : Length := 0
  right : Length := 0
  bottom : Length := 0
  left : Length := 0
deriving Repr, BEq, Inhabited

namespace EdgeInsets

def zero : EdgeInsets := {}

def uniform (v : Length) : EdgeInsets :=
  { top := v, right := v, bottom := v, left := v }

def symmetric (horizontal vertical : Length) : EdgeInsets :=
  { top := vertical, right := horizontal, bottom := vertical, left := horizontal }

def horizontal (e : EdgeInsets) : Length := e.left + e.right
def vertical (e : EdgeInsets) : Length := e.top + e.bottom

def total (e : EdgeInsets) : Length := e.horizontal + e.vertical

end EdgeInsets

/-- How content scales to fit its container. -/
inductive ContentScaleMode where
  | contain   -- Fit inside, maintain aspect ratio (may leave gaps)
  | cover     -- Fill completely, maintain aspect ratio (may clip)
  | stretch   -- Stretch to fill (distorts aspect ratio)
deriving Repr, BEq, Inhabited

/-- Anchor point for scaled content within its container. -/
inductive ScaleAnchor where
  | center
  | topLeft
  | top
  | topRight
  | left
  | right
  | bottomLeft
  | bottom
  | bottomRight
deriving Repr, BEq, Inhabited

/-- Which area responds to hit testing for scaled content. -/
inductive ScaleHitArea where
  | scaled     -- Only hit the actual scaled content bounds
  | container  -- Hit anywhere in the container
deriving Repr, BEq, Inhabited

/-- Content scaling configuration for containers. -/
structure ContentScale where
  mode : ContentScaleMode := .contain
  allowUpscale : Bool := false
  anchor : ScaleAnchor := .center
  hitArea : ScaleHitArea := .scaled
deriving Repr, BEq, Inhabited

namespace ContentScale

def default : ContentScale := {}

def contain : ContentScale := { mode := .contain }
def cover : ContentScale := { mode := .cover }
def stretch : ContentScale := { mode := .stretch }

def withUpscale (cs : ContentScale) : ContentScale :=
  { cs with allowUpscale := true }

def withAnchor (cs : ContentScale) (anchor : ScaleAnchor) : ContentScale :=
  { cs with anchor := anchor }

def withContainerHit (cs : ContentScale) : ContentScale :=
  { cs with hitArea := .container }

end ContentScale

/-- Box model constraints for a layout node. -/
structure BoxConstraints where
  width : Dimension := .auto
  height : Dimension := .auto
  minWidth : Length := 0
  maxWidth : Option Length := none
  minHeight : Length := 0
  maxHeight : Option Length := none
  margin : EdgeInsets := {}
  padding : EdgeInsets := {}
deriving Repr, BEq, Inhabited

namespace BoxConstraints

def empty : BoxConstraints := {}

/-- Create constraints with explicit width and height. -/
def fixed (width height : Length) : BoxConstraints :=
  { width := .length width, height := .length height }

/-- Create constraints with explicit width only. -/
def fixedWidth (width : Length) : BoxConstraints :=
  { width := .length width }

/-- Create constraints with explicit height only. -/
def fixedHeight (height : Length) : BoxConstraints :=
  { height := .length height }

/-- Clamp a value to min/max constraints. -/
def clampWidth (c : BoxConstraints) (w : Length) : Length :=
  let clamped := max c.minWidth w
  match c.maxWidth with
  | some maxW => min clamped maxW
  | none => clamped

/-- Clamp a value to min/max constraints. -/
def clampHeight (c : BoxConstraints) (h : Length) : Length :=
  let clamped := max c.minHeight h
  match c.maxHeight with
  | some maxH => min clamped maxH
  | none => clamped

end BoxConstraints

end Trellis
