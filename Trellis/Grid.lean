/-
  Trellis Grid Types
  CSS Grid container and item properties.
-/
import Trellis.Types
import Trellis.Flex  -- for AlignItems reuse

namespace Trellis

/-- Size of a grid track (row or column). -/
inductive TrackSize where
  | fixed (d : Dimension)           -- Fixed dimension (length, percent, auto)
  | fr (n : Float)                  -- Fractional unit (flexible space)
  | minmax (min max : TrackSize)    -- minmax(min, max) function
  | fitContent (max : Length)       -- fit-content(max) function
  | subgrid                         -- Inherit tracks from parent grid
deriving Repr, BEq, Inhabited

namespace TrackSize

def auto : TrackSize := .fixed .auto
def px (n : Length) : TrackSize := .fixed (.length n)
def percent (p : Float) : TrackSize := .fixed (.percent p)

/-- Check if this track size uses fr units. -/
def isFr : TrackSize → Bool
  | .fr _ => true
  | .minmax _ max => max.isFr
  | _ => false

/-- Get the fr value if this is an fr track. -/
def frValue : TrackSize → Float
  | .fr n => n
  | _ => 0

end TrackSize

/-- Repeat mode for CSS repeat() function. -/
inductive RepeatMode where
  | count (n : Nat)  -- repeat(n, ...) - repeat exactly n times
  | autoFill         -- repeat(auto-fill, ...) - fill container with tracks
  | autoFit          -- repeat(auto-fit, ...) - fill and collapse empty tracks
deriving Repr, BEq, Inhabited

/-- A single grid track definition. -/
structure GridTrack where
  size : TrackSize := .fixed .auto
  startLineNames : Array String := #[]  -- Line names before this track
  endLineNames : Array String := #[]    -- Line names after this track
deriving Repr, BEq, Inhabited

/-- A track entry in a template (single track or repeat). -/
inductive TrackEntry where
  | single (track : GridTrack)
  | repeat (mode : RepeatMode) (tracks : Array GridTrack)
deriving Repr, BEq, Inhabited

namespace TrackEntry

/-- Create a single track entry from a size. -/
def fromSize (size : TrackSize) : TrackEntry :=
  .single { size }

/-- Create a repeat entry with a count. -/
def repeated (n : Nat) (sizes : Array TrackSize) : TrackEntry :=
  .repeat (.count n) (sizes.map fun size => { size })

/-- Create an auto-fill repeat entry. -/
def autoFill (sizes : Array TrackSize) : TrackEntry :=
  .repeat .autoFill (sizes.map fun size => { size })

/-- Create an auto-fit repeat entry. -/
def autoFit (sizes : Array TrackSize) : TrackEntry :=
  .repeat .autoFit (sizes.map fun size => { size })

end TrackEntry

/-- Grid template for rows or columns. -/
structure GridTemplate where
  /-- Track entries (may include repeat() functions). -/
  entries : Array TrackEntry := #[]
  /-- Legacy: direct track array (for backwards compatibility). -/
  tracks : Array GridTrack := #[]
  /-- Size for auto-generated (implicit) tracks. -/
  autoSize : TrackSize := .fixed .auto
deriving Repr, BEq, Inhabited

namespace GridTemplate

def empty : GridTemplate := {}

/-- True if this template declares subgrid tracks. -/
def isSubgrid (template : GridTemplate) : Bool :=
  if !template.entries.isEmpty then
    template.entries.size == 1 && match template.entries[0]! with
      | .single track => track.size == .subgrid
      | _ => false
  else
    template.tracks.size == 1 && template.tracks[0]!.size == .subgrid

/-- Create a subgrid template. -/
def subgrid : GridTemplate :=
  { entries := #[.single { size := .subgrid }] }

/-- Create a template from track sizes (legacy, no repeats). -/
def fromSizes (sizes : Array TrackSize) : GridTemplate :=
  { tracks := sizes.map fun size => { size } }

/-- Create a template from track entries (supports repeats). -/
def fromEntries (entries : Array TrackEntry) : GridTemplate :=
  { entries }

/-- Create a template with n equal fr columns/rows. -/
def repeated (n : Nat) (size : TrackSize := .fr 1) : GridTemplate :=
  { tracks := (List.replicate n { size := size }).toArray }

/-- Create a template with specific pixel sizes. -/
def pixels (sizes : Array Length) : GridTemplate :=
  { tracks := sizes.map fun s => { size := .fixed (.length s) } }

/-- Create a template with a repeat() function. -/
def withRepeat (mode : RepeatMode) (sizes : Array TrackSize) : GridTemplate :=
  { entries := #[.repeat mode (sizes.map fun size => { size })] }

/-- Create a template with repeat(n, size). -/
def repeatCount (n : Nat) (sizes : Array TrackSize) : GridTemplate :=
  { entries := #[.repeat (.count n) (sizes.map fun size => { size })] }

/-- Create a template with repeat(auto-fill, sizes). -/
def autoFill (sizes : Array TrackSize) : GridTemplate :=
  { entries := #[.repeat .autoFill (sizes.map fun size => { size })] }

/-- Create a template with repeat(auto-fit, sizes). -/
def autoFit (sizes : Array TrackSize) : GridTemplate :=
  { entries := #[.repeat .autoFit (sizes.map fun size => { size })] }

end GridTemplate

/-- Grid template areas for semantic grid definitions. -/
structure GridTemplateAreas where
  /-- Rows of area names; `none` represents an empty cell. -/
  rows : Array (Array (Option String)) := #[]
deriving Repr, BEq, Inhabited

namespace GridTemplateAreas

def empty : GridTemplateAreas := {}

/-- Create template areas from explicit rows. -/
def fromRows (rows : Array (Array (Option String))) : GridTemplateAreas :=
  { rows }

/-- Create template areas from string rows, where "." denotes an empty cell. -/
def fromStrings (rows : Array (Array String)) : GridTemplateAreas :=
  { rows := rows.map (·.map fun s => if s == "." then none else some s) }

/-- Number of rows defined by template areas. -/
def rowCount (areas : GridTemplateAreas) : Nat :=
  areas.rows.size

/-- Maximum column count across all area rows. -/
def colCount (areas : GridTemplateAreas) : Nat :=
  areas.rows.foldl (fun acc row => max acc row.size) 0

end GridTemplateAreas

/-- Reference to a grid line for item placement. -/
inductive GridLine where
  | auto               -- Automatic placement
  | line (n : Int)     -- Numeric line (1-indexed, negative from end)
  | span (n : Nat)     -- Span n tracks from start position
  | named (s : String) -- Named line reference
deriving Repr, BEq, Inhabited

/-- Placement of a grid item in one dimension. -/
structure GridSpan where
  start : GridLine := .auto
  finish : GridLine := .auto  -- Using 'finish' to avoid 'end' keyword
deriving Repr, BEq, Inhabited

namespace GridSpan

def auto : GridSpan := {}

/-- Place at specific line numbers. -/
def lines (startLine endLine : Int) : GridSpan :=
  { start := .line startLine, finish := .line endLine }

/-- Span a number of tracks from auto placement. -/
def spanTracks (n : Nat) : GridSpan :=
  { start := .auto, finish := .span n }

/-- Start at a line and span tracks. -/
def lineSpan (startLine : Int) (spanCount : Nat) : GridSpan :=
  { start := .line startLine, finish := .span spanCount }

end GridSpan

/-- Placement of a grid item. -/
structure GridPlacement where
  row : GridSpan := {}
  column : GridSpan := {}
deriving Repr, BEq, Inhabited

namespace GridPlacement

def auto : GridPlacement := {}

/-- Place at specific row and column lines. -/
def atPosition (row col : Int) : GridPlacement :=
  { row := { start := .line row }, column := { start := .line col } }

/-- Place spanning multiple rows and columns. -/
def span (rowSpan colSpan : Nat) : GridPlacement :=
  { row := GridSpan.spanTracks rowSpan, column := GridSpan.spanTracks colSpan }

end GridPlacement

/-- Direction for auto-placement of grid items. -/
inductive GridAutoFlow where
  | row         -- Fill rows first (default)
  | column      -- Fill columns first
  | rowDense    -- Fill rows, backfill gaps
  | columnDense -- Fill columns, backfill gaps
deriving Repr, BEq, Inhabited

/-- Properties for a grid container. -/
structure GridContainer where
  templateRows : GridTemplate := {}
  templateColumns : GridTemplate := {}
  templateAreas : GridTemplateAreas := {}
  rowGap : Length := 0
  columnGap : Length := 0
  justifyItems : AlignItems := .stretch   -- Alignment within cells (inline axis)
  alignItems : AlignItems := .stretch     -- Alignment within cells (block axis)
  justifyContent : JustifyContent := .flexStart  -- Alignment of grid in container
  alignContent : AlignContent := .flexStart
  autoFlow : GridAutoFlow := .row
  autoRows : TrackSize := .fixed .auto    -- Size for implicit rows
  autoColumns : TrackSize := .fixed .auto -- Size for implicit columns
deriving Repr, BEq, Inhabited

namespace GridContainer

def default : GridContainer := {}

/-- Create a simple grid with fixed column count. -/
def columns (n : Nat) (gap : Length := 0) : GridContainer :=
  { templateColumns := GridTemplate.repeated n
    columnGap := gap
    rowGap := gap }

/-- Create a grid with specific column sizes. -/
def withColumns (sizes : Array TrackSize) (gap : Length := 0) : GridContainer :=
  { templateColumns := GridTemplate.fromSizes sizes
    columnGap := gap
    rowGap := gap }

/-- Create a grid with specific row and column sizes. -/
def withTemplate (rows cols : Array TrackSize) (gap : Length := 0) : GridContainer :=
  { templateRows := GridTemplate.fromSizes rows
    templateColumns := GridTemplate.fromSizes cols
    rowGap := gap
    columnGap := gap }

end GridContainer

/-- Properties for a grid item (child of grid container). -/
structure GridItem where
  placement : GridPlacement := {}
  area : Option String := none            -- Optional grid area name
  justifySelf : Option AlignItems := none  -- Override container's justifyItems
  alignSelf : Option AlignItems := none    -- Override container's alignItems
deriving Repr, BEq, Inhabited

namespace GridItem

def default : GridItem := {}

/-- Place at specific grid position. -/
def atPosition (row col : Int) : GridItem :=
  { placement := GridPlacement.atPosition row col }

/-- Place spanning multiple cells. -/
def span (rowSpan colSpan : Nat) : GridItem :=
  { placement := GridPlacement.span rowSpan colSpan }

/-- Place an item in a named grid area. -/
def inArea (name : String) : GridItem :=
  { area := some name }

end GridItem

end Trellis
