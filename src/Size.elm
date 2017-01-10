module Size exposing (..)

type alias Size a =
  { width : a
  , height : a
  }

type alias Map = Size Int
type alias Tile = Size Int
type alias Render = Size Int

map : (from -> to) -> Size from -> Size to
map f {width, height} = { width = f width, height = f height }
