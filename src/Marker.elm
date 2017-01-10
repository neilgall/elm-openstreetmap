module Marker exposing (Marker, Projection, view)

import Html
import Html.Attributes as Attributes
import LatLon
import Message exposing (Msg)
import Point
import Render

type alias Marker =
  { position : LatLon.Position
  , title : String
  }

type alias Projection = LatLon.Position -> Point.Render

view : Projection -> Marker -> Html.Html Msg
view project {position, title} =
  Html.img
    [ Attributes.src "/images/marker.png"
    , Attributes.style
        [ ("position", "absolute")
        , ("transform", project position |> Render.translate3d)    
        ]
    ]
    []
