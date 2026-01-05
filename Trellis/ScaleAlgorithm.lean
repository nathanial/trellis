/-
  Trellis Scale Algorithm
  Compute scale transforms for content scaling.
-/
import Trellis.Types
import Trellis.Result

namespace Trellis

/-- Compute anchor offset to position scaled content within available space.
    Returns (offsetX, offsetY) from the container's content origin. -/
def computeAnchorOffset (anchor : ScaleAnchor) (availW availH scaledW scaledH : Float)
    : Float × Float :=
  match anchor with
  | .center      => ((availW - scaledW) / 2, (availH - scaledH) / 2)
  | .topLeft     => (0, 0)
  | .top         => ((availW - scaledW) / 2, 0)
  | .topRight    => (availW - scaledW, 0)
  | .left        => (0, (availH - scaledH) / 2)
  | .right       => (availW - scaledW, (availH - scaledH) / 2)
  | .bottomLeft  => (0, availH - scaledH)
  | .bottom      => ((availW - scaledW) / 2, availH - scaledH)
  | .bottomRight => (availW - scaledW, availH - scaledH)

/-- Compute scale metadata for content scaling.
    Given a ContentScale configuration, available container size, and intrinsic content size,
    returns ScaleMetadata with computed scale factors and anchor offset. -/
def computeScaleMetadata (cs : ContentScale) (availW availH intrinsicW intrinsicH : Float)
    : ScaleMetadata :=
  -- Avoid division by zero
  let intrinsicW := max intrinsicW 1.0
  let intrinsicH := max intrinsicH 1.0

  let (scaleX, scaleY) := match cs.mode with
    | .contain =>
      -- Fit inside: use minimum scale to preserve aspect ratio
      let s := min (availW / intrinsicW) (availH / intrinsicH)
      let s := if cs.allowUpscale then s else min s 1.0
      (s, s)
    | .cover =>
      -- Fill completely: use maximum scale to preserve aspect ratio (may clip)
      let s := max (availW / intrinsicW) (availH / intrinsicH)
      let s := if cs.allowUpscale then s else min s 1.0
      (s, s)
    | .stretch =>
      -- Stretch to fill: scale each axis independently (may distort)
      let sx := availW / intrinsicW
      let sy := availH / intrinsicH
      let sx := if cs.allowUpscale then sx else min sx 1.0
      let sy := if cs.allowUpscale then sy else min sy 1.0
      (sx, sy)

  let scaledW := intrinsicW * scaleX
  let scaledH := intrinsicH * scaleY
  let (offsetX, offsetY) := computeAnchorOffset cs.anchor availW availH scaledW scaledH

  { scaleX := scaleX
    scaleY := scaleY
    offsetX := offsetX
    offsetY := offsetY
    intrinsicWidth := intrinsicW
    intrinsicHeight := intrinsicH
    hitArea := cs.hitArea }

/-- Check if a point is inside the scaled content bounds.
    Point coordinates are relative to container's content rect origin. -/
def ScaleMetadata.containsPoint (m : ScaleMetadata) (px py : Float) : Bool :=
  let scaledW := m.intrinsicWidth * m.scaleX
  let scaledH := m.intrinsicHeight * m.scaleY
  px >= m.offsetX && px <= m.offsetX + scaledW &&
  py >= m.offsetY && py <= m.offsetY + scaledH

/-- Transform a point from container coordinates to child/intrinsic coordinates.
    Input: point relative to container's content rect origin.
    Output: point in the intrinsic coordinate space of the scaled content. -/
def ScaleMetadata.transformPointToChild (m : ScaleMetadata) (px py : Float) : Float × Float :=
  let localX := (px - m.offsetX) / m.scaleX
  let localY := (py - m.offsetY) / m.scaleY
  (localX, localY)

/-- Transform a point from child/intrinsic coordinates to container coordinates.
    Input: point in the intrinsic coordinate space.
    Output: point relative to container's content rect origin. -/
def ScaleMetadata.transformPointToContainer (m : ScaleMetadata) (px py : Float) : Float × Float :=
  let containerX := px * m.scaleX + m.offsetX
  let containerY := py * m.scaleY + m.offsetY
  (containerX, containerY)

end Trellis
