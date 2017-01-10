module Message exposing (Msg(..))

import Mouse
import Size

type Msg
  = MapResize Size.Map
  | DragStart Mouse.Position
  | DragAt Mouse.Position
  | DragEnd Mouse.Position
  | ZoomIn
  | ZoomOut

