module Render exposing (..)

import Point
import Util

px : Int -> String
px n = toString n ++ "px"

translate3d : Point.Render -> String
translate3d pos =
  Util.stringWithSubstitutions "translate3d({x}px, {y}px, 0px)"
    [ ("{x}", toString pos.x)
    , ("{y}", toString pos.y) ]
      
