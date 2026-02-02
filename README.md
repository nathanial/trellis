# Trellis

A pure CSS layout library for Lean 4, implementing Flexbox and Grid layout algorithms.

Trellis provides CSS-style layout computation without any rendering dependencies. Given a tree of layout nodes with constraints, it computes the final position and size of each element.

## Features

- **Flexbox Layout**: Full flex container support with `flex-direction`, `flex-wrap`, `justify-content`, `align-items`, `align-content`, gaps, and flex item properties (`grow`, `shrink`, `basis`)
- **Grid Layout**: CSS Grid with template rows/columns, `fr` units, auto-placement, explicit positioning, spanning, cell alignment, named lines (boundary-based), and subgrid
- **Box Model**: Margin, padding, min/max constraints, and CSS dimension values (`auto`, `length`, `percent`, `min-content`, `max-content`)
- **Pure Computation**: No side effects, no rendering - just layout math

## Installation

Add to your `lakefile.lean`:

```lean
require trellis from git "https://github.com/nathanial/trellis" @ "v0.0.1"
```


## Quick Start

```lean
import Trellis

open Trellis

-- Create a horizontal row with three items
def myLayout : LayoutNode :=
  LayoutNode.row 0 #[
    LayoutNode.leaf 1 ⟨100, 50⟩,  -- 100x50 content
    LayoutNode.leaf 2 ⟨150, 50⟩,
    LayoutNode.leaf 3 ⟨120, 50⟩
  ] (gap := 10)

-- Compute layout in a 500x200 container
def result : LayoutResult := layout myLayout 500 200

-- Get computed position/size for node 2
def node2Layout : ComputedLayout := result.get! 2
-- node2Layout.x = 110 (100 + 10 gap)
-- node2Layout.width = 150
```

## Usage

### Flexbox Layout

```lean
-- Row container with centered items
let container := LayoutNode.flexBox 0
  { FlexContainer.row with
    justifyContent := .center
    alignItems := .center
    gap := 16 }
  #[
    LayoutNode.leaf 1 ⟨80, 40⟩,
    LayoutNode.leaf 2 ⟨80, 40⟩
  ]

-- Column with items that grow to fill space
let growingColumn := LayoutNode.flexBox 0 (FlexContainer.column) #[
  LayoutNode.leaf' 1 0 0 {} (.flexChild (FlexItem.growing 1)),
  LayoutNode.leaf' 2 0 0 {} (.flexChild (FlexItem.growing 2))
]
-- In a 300px tall container: item 1 gets 100px, item 2 gets 200px

-- Wrapping row
let wrappingRow := LayoutNode.flexBox 0
  (FlexContainer.rowWrap (gap := 8) (rowGap := 8))
  children
```

### Flex Item Properties

```lean
-- Item that grows to fill available space
.flexChild (FlexItem.growing 1)

-- Item with specific grow/shrink factors
.flexChild (FlexItem.flexing 2 0.5)

-- Item with fixed basis that doesn't flex
.flexChild (FlexItem.fixed 200)

-- Full control
.flexChild {
  grow := 1
  shrink := 0
  basis := .length 100
  alignSelf := some .center
}
```

### Grid Layout

```lean
-- 3-column grid with equal widths
let grid := LayoutNode.grid 0 3 children (gap := 10)

-- Grid with specific column sizes
let customGrid := LayoutNode.gridBox 0
  (GridContainer.withColumns #[.px 100, .fr 1, .fr 2])
  children

-- Full template with rows and columns
let fullGrid := LayoutNode.gridBox 0
  (GridContainer.withTemplate
    #[.px 50, .fr 1]           -- rows: 50px fixed, then flexible
    #[.fr 1, .fr 1, .fr 1]     -- 3 equal columns
    (gap := 8))
  children
```

### Subgrid

```lean
-- Parent grid defines the column tracks
let parent := LayoutNode.gridBox 0
  { GridContainer.default with
    templateRows := GridTemplate.fromSizes #[.auto]
    templateColumns := GridTemplate.fromSizes #[.px 120, .px 80]
    columnGap := 12
  }
  #[
    -- Child grid inherits the parent's columns via subgrid
    LayoutNode.gridBox 1
      { GridContainer.default with
        templateRows := GridTemplate.fromSizes #[.auto]
        templateColumns := GridTemplate.subgrid
      }
      #[
        LayoutNode.leaf' 2 120 24 {} (.gridChild (GridItem.atPosition 1 1)),
        LayoutNode.leaf' 3 80 24 {} (.gridChild (GridItem.atPosition 1 2))
      ] {} (.gridChild (GridItem.span 1 2))
  ]
```

### Grid Item Placement

```lean
-- Place at specific grid position (1-indexed)
.gridChild (GridItem.atPosition 2 3)  -- row 2, column 3

-- Span multiple cells
.gridChild (GridItem.span 2 3)  -- 2 rows, 3 columns

-- Detailed placement
.gridChild {
  placement := {
    row := GridSpan.lineSpan 1 2     -- start at line 1, span 2
    column := GridSpan.lines 2 4     -- columns 2-4
  }
  justifySelf := some .center
  alignSelf := some .flexEnd
}
```

### Named Lines (Boundary-Based)

```lean
-- Line names live on boundaries between tracks.
-- startLineNames apply before the track; endLineNames apply after the track.
let cols : GridTemplate := GridTemplate.fromEntries #[
  .single { size := .px 120, startLineNames := #["left"], endLineNames := #["mid"] },
  .single { size := .px 80, endLineNames := #["right"] }
]

let namedGrid := LayoutNode.gridBox 0
  { GridContainer.default with
    templateRows := GridTemplate.fromSizes #[.auto]
    templateColumns := cols
  }
  #[
    LayoutNode.leaf' 1 0 20 {} (.gridChild {
      placement := { column := { start := .named "mid", finish := .named "right" } }
    })
  ]
```

### Box Constraints

```lean
-- Fixed size
BoxConstraints.fixed 200 100

-- With min/max
{ BoxConstraints.empty with
  width := .percent 0.5
  minWidth := 100
  maxWidth := some 400
  margin := EdgeInsets.uniform 8
  padding := EdgeInsets.symmetric 16 8
}
```

### Nested Layouts

```lean
-- Mix flex and grid containers
let dashboard := LayoutNode.column 0 #[
  -- Header (fixed height)
  LayoutNode.leaf' 1 0 0 (BoxConstraints.fixedHeight 60) (.flexChild FlexItem.default),

  -- Content area (grows)
  LayoutNode.gridBox 2 (GridContainer.columns 3 (gap := 16)) #[
    LayoutNode.leaf 3 ⟨0, 100⟩,
    LayoutNode.leaf 4 ⟨0, 100⟩,
    LayoutNode.leaf 5 ⟨0, 100⟩
  ] {} (.flexChild (FlexItem.growing 1))
]
```

## API Reference

### Core Types

| Type | Description |
|------|-------------|
| `LayoutNode` | Node in the layout tree (container or leaf) |
| `LayoutResult` | Collection of computed layouts |
| `ComputedLayout` | Position and size for a single node |
| `LayoutRect` | Rectangle with x, y, width, height |

### Dimension Values

| Value | Description |
|-------|-------------|
| `.auto` | Size from content or context |
| `.length n` | Explicit pixel value |
| `.percent p` | Percentage (0.0 to 1.0) |
| `.minContent` | Minimum content size |
| `.maxContent` | Maximum content size |

### Flex Properties

| Property | Values |
|----------|--------|
| `direction` | `.row`, `.rowReverse`, `.column`, `.columnReverse` |
| `wrap` | `.nowrap`, `.wrap`, `.wrapReverse` |
| `justifyContent` | `.flexStart`, `.flexEnd`, `.center`, `.spaceBetween`, `.spaceAround`, `.spaceEvenly` |
| `alignItems` | `.flexStart`, `.flexEnd`, `.center`, `.stretch`, `.baseline` |
| `alignContent` | Same as `justifyContent` plus `.stretch` |

### Grid Track Sizes

| Size | Description |
|------|-------------|
| `.px n` | Fixed pixel size |
| `.fr n` | Fractional unit |
| `.percent p` | Percentage |
| `.auto` | Auto-sized |
| `.minmax min max` | Clamp between min and max |
| `.fitContent max` | Fit content up to max |

## Module Structure

```
Trellis/
├── Types.lean      # Dimension, EdgeInsets, BoxConstraints
├── Flex.lean       # FlexContainer, FlexItem, alignment enums
├── Grid.lean       # GridContainer, GridItem, TrackSize, GridPlacement
├── Axis.lean       # Axis abstraction for direction-agnostic code
├── Node.lean       # LayoutNode tree structure
├── Result.lean     # LayoutRect, ComputedLayout, LayoutResult
└── Algorithm.lean  # Layout computation (flex and grid algorithms)
```

## Building

```bash
lake build
```

## Testing

```bash
lake test
```

Tests cover:
- Basic leaf node layout
- Flex row/column placement
- Flex grow/shrink distribution
- Justify and align content
- Nested containers
- Grid columns with fr units
- Grid gaps
- Grid auto-placement
- Grid explicit positioning
- Grid spanning
- Grid cell alignment

## Requirements

- Lean 4.26.0+
- crucible (for tests)

## License

MIT License - see [LICENSE](LICENSE)
