/-
  Trellis Layout Tests - Content Scale Test
  Unit tests for the content scaling metadata (contain, cover, stretch)
  and coordinate transformations.
-/
import Crucible
import Trellis

namespace TrellisTests.LayoutTests.ContentScale

open Crucible
open Trellis

testSuite "Trellis Layout Tests - Content Scale"

/-! ## Content Scale Tests -/

test "computeScaleMetadata contain mode scales down uniformly" := do
  -- Content 400x300 in container 200x200
  -- Contain mode: min(200/400, 200/300) = min(0.5, 0.67) = 0.5
  let cs : ContentScale := { mode := .contain }
  let m := computeScaleMetadata cs 200 200 400 300
  shouldBeNear m.scaleX 0.5 0.01
  shouldBeNear m.scaleY 0.5 0.01
  -- Scaled size: 400*0.5=200, 300*0.5=150
  -- Centered in 200x200: offset = (200-200)/2=0, (200-150)/2=25
  shouldBeNear m.offsetX 0 0.01
  shouldBeNear m.offsetY 25 0.01

test "computeScaleMetadata contain mode does not upscale by default" := do
  -- Content 100x100 in container 400x400
  -- Would upscale to 4x, but allowUpscale=false
  let cs : ContentScale := { mode := .contain, allowUpscale := false }
  let m := computeScaleMetadata cs 400 400 100 100
  shouldBeNear m.scaleX 1.0 0.01
  shouldBeNear m.scaleY 1.0 0.01
  -- Content stays at 100x100, centered in 400x400: offset = (400-100)/2 = 150
  shouldBeNear m.offsetX 150 0.01
  shouldBeNear m.offsetY 150 0.01

test "computeScaleMetadata contain mode upscales when allowed" := do
  -- Content 100x100 in container 400x400
  -- allowUpscale=true, so scale = min(400/100, 400/100) = 4.0
  let cs : ContentScale := { mode := .contain, allowUpscale := true }
  let m := computeScaleMetadata cs 400 400 100 100
  shouldBeNear m.scaleX 4.0 0.01
  shouldBeNear m.scaleY 4.0 0.01
  -- Scaled size fills container: 400x400, offset = 0,0
  shouldBeNear m.offsetX 0 0.01
  shouldBeNear m.offsetY 0 0.01

test "computeScaleMetadata cover mode scales to fill" := do
  -- Content 400x300 in container 200x200
  -- Cover mode: max(200/400, 200/300) = max(0.5, 0.67) = 0.67
  let cs : ContentScale := { mode := .cover }
  let m := computeScaleMetadata cs 200 200 400 300
  shouldBeNear m.scaleX 0.666 0.01
  shouldBeNear m.scaleY 0.666 0.01

test "computeScaleMetadata stretch mode scales non-uniformly" := do
  -- Content 400x200 in container 200x200
  -- Stretch: scaleX = 200/400 = 0.5, scaleY = 200/200 = 1.0
  let cs : ContentScale := { mode := .stretch }
  let m := computeScaleMetadata cs 200 200 400 200
  shouldBeNear m.scaleX 0.5 0.01
  shouldBeNear m.scaleY 1.0 0.01

test "computeScaleMetadata topLeft anchor" := do
  -- Content 200x200 in container 400x400, contain mode
  -- With allowUpscale=false, scale=1.0, content stays at 200x200
  let cs : ContentScale := { mode := .contain, anchor := .topLeft }
  let m := computeScaleMetadata cs 400 400 200 200
  shouldBeNear m.offsetX 0 0.01
  shouldBeNear m.offsetY 0 0.01

test "computeScaleMetadata bottomRight anchor" := do
  -- Content 200x200 in container 400x400, contain mode
  -- scale=1.0, content 200x200, offset = (400-200, 400-200) = (200, 200)
  let cs : ContentScale := { mode := .contain, anchor := .bottomRight }
  let m := computeScaleMetadata cs 400 400 200 200
  shouldBeNear m.offsetX 200 0.01
  shouldBeNear m.offsetY 200 0.01

test "computeScaleMetadata stores intrinsic size" := do
  let cs : ContentScale := { mode := .contain }
  let m := computeScaleMetadata cs 200 200 400 300
  shouldBeNear m.intrinsicWidth 400 0.01
  shouldBeNear m.intrinsicHeight 300 0.01

test "ScaleMetadata.containsPoint checks scaled bounds" := do
  -- Scale metadata: scale 0.5, offset (25, 25), intrinsic 200x200
  -- Scaled bounds: (25, 25) to (25+200*0.5, 25+200*0.5) = (25, 25) to (125, 125)
  let m : ScaleMetadata := {
    scaleX := 0.5, scaleY := 0.5
    offsetX := 25, offsetY := 25
    intrinsicWidth := 200, intrinsicHeight := 200
  }
  -- Point inside scaled bounds
  shouldSatisfy (m.containsPoint 50 50) "point (50,50) should be inside"
  shouldSatisfy (m.containsPoint 25 25) "point (25,25) should be inside (corner)"
  shouldSatisfy (m.containsPoint 125 125) "point (125,125) should be inside (corner)"
  -- Point outside scaled bounds
  shouldSatisfy (!m.containsPoint 0 0) "point (0,0) should be outside"
  shouldSatisfy (!m.containsPoint 150 150) "point (150,150) should be outside"

test "ScaleMetadata.transformPointToChild inverts scale" := do
  -- Scale metadata: scale 0.5, offset (50, 50)
  -- Container point (100, 100) -> child point
  -- localX = (100 - 50) / 0.5 = 100
  -- localY = (100 - 50) / 0.5 = 100
  let m : ScaleMetadata := {
    scaleX := 0.5, scaleY := 0.5
    offsetX := 50, offsetY := 50
    intrinsicWidth := 200, intrinsicHeight := 200
  }
  let (lx, ly) := m.transformPointToChild 100 100
  shouldBeNear lx 100 0.01
  shouldBeNear ly 100 0.01

test "ScaleMetadata.transformPointToContainer applies scale" := do
  -- Scale metadata: scale 0.5, offset (50, 50)
  -- Child point (100, 100) -> container point
  -- containerX = 100 * 0.5 + 50 = 100
  -- containerY = 100 * 0.5 + 50 = 100
  let m : ScaleMetadata := {
    scaleX := 0.5, scaleY := 0.5
    offsetX := 50, offsetY := 50
    intrinsicWidth := 200, intrinsicHeight := 200
  }
  let (cx, cy) := m.transformPointToContainer 100 100
  shouldBeNear cx 100 0.01
  shouldBeNear cy 100 0.01

test "ScaleMetadata transform round-trip" := do
  -- Transform to child and back should give original point
  let m : ScaleMetadata := {
    scaleX := 0.75, scaleY := 0.5
    offsetX := 30, offsetY := 40
    intrinsicWidth := 200, intrinsicHeight := 200
  }
  let (lx, ly) := m.transformPointToChild 80 90
  let (cx, cy) := m.transformPointToContainer lx ly
  shouldBeNear cx 80 0.01
  shouldBeNear cy 90 0.01

#generate_tests

end TrellisTests.LayoutTests.ContentScale
