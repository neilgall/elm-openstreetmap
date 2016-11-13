module Geometry exposing (..)

type alias Size a =
  { width : a
  , height : a
  }

type alias MapSize = Size Int
type alias TileSize = Size Int

mapSize : (number -> number') -> Size number -> Size number'
mapSize f {width, height} = { width = f width, height = f height }


type alias Point a =
  { x : a
  , y : a
  }

type alias Tile = Point Int
type alias MapPoint = Point Float

translatePoint : Point number -> number -> number -> Point number
translatePoint {x, y} dx dy = { x = x + dx, y = y + dy }

mapPoint : (number -> number') -> Point number -> Point number'
mapPoint f {x, y} = { x = f x, y = f y }
