module Projection exposing (..)

import Point
import Size
import Util exposing (frac)

type alias Projection = Point.Map -> Point.Render

-- Render coordinate for a Point.Map relative to the centre of the rendered map
project : Size.Render -> Size.Tile -> Point.Map -> Projection
project renderSize tileSize mapCentre pos =
  let
    scale i f = round ((toFloat i) * f)
    centref = Point.map frac mapCentre
    x = renderSize.width // 2 + scale tileSize.width (pos.x - centref.x)
    y = renderSize.height // 2 + scale tileSize.height (pos.y - centref.y)
  in
    { x=x, y=y }
