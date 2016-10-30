module Geometry exposing (..)

import Mouse

type alias Point = Mouse.Position

type alias Size =
  { width : Int
  , height : Int
  }

translatePoint : Point -> Int -> Int -> Point
translatePoint {x,y} dx dy = { x = x + dx, y = y + dy }
