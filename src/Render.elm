module Render exposing (..)

import Html
import Html.Attributes as Attributes
import Point
import Util

px : Int -> String
px n = toString n ++ "px"

translate3d : Point.Render -> String
translate3d pos =
  Util.stringWithSubstitutions "translate3d({x}px, {y}px, 0px)"
    [ ("{x}", toString pos.x)
    , ("{y}", toString pos.y) ]
      
fullSize : Html.Attribute msg
fullSize =
  Attributes.style
    [ ("width", "100%")
    , ("height", "100%")
    , ("position", "relative")
    , ("overflow", "hidden")
    ]
