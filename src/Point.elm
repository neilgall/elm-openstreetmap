module Point exposing (..)

type alias Point a =
  { x : a
  , y : a
  }

type alias Map = Point Float
type alias Render = Point Int
type alias Tile = Point Int

translate : Point number -> number -> number -> Point number
translate {x, y} dx dy = { x = x + dx, y = y + dy }

map : (from -> to) -> Point from -> Point to
map f {x, y} = { x = f x, y = f y }
