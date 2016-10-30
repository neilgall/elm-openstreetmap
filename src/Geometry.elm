module Geometry exposing (Size, Position)

import Mouse

type alias Position = Mouse.Position

type alias Size =
  { width : Int
  , height : Int
  }
