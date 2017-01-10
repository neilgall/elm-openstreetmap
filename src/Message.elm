module Message exposing (Msg(..))

import Mouse
import Geometry exposing (..)

type Msg
  = MapResize MapSize
  | DragStart Mouse.Position
  | DragAt Mouse.Position
  | DragEnd Mouse.Position
  | ZoomIn
  | ZoomOut

