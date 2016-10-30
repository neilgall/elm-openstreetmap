module Projection exposing
  ( LatLon
  , Zoom
  , pointFromLatLon
  , latLonFromPoint
  )

import Geometry exposing (..)

type alias LatLon =
  { latitude : Float
  , longitude : Float
  }

type alias Zoom = Int

-- see https://wiki.openstreetmap.org/wiki/Slippy_map_tilenames

pointFromLatLon : Zoom -> LatLon -> Point
pointFromLatLon zoom {latitude, longitude} =
  let
    scale = 2^zoom |> toFloat
    lat' = degreesToRadians latitude
    x = scale * (longitude + 180) / 360
    y = scale * (1 - (logBase e (tan lat' + sec lat')) / pi) / 2
  in
    { x = floor x, y = floor y }

latLonFromPoint : Zoom -> Point -> LatLon
latLonFromPoint zoom {x, y} =
  let
    scale = 2^zoom |> toFloat
    x' = toFloat x + 0.5
    y' = toFloat y + 0.5
    lon = x' / scale * 360 - 180
    lat' = sinh (pi * (1 - 2 * y' / scale)) |> atan
    lat = radiansToDegrees lat'
  in
    { latitude = lat, longitude = lon }

sec : Float -> Float
sec a = 1 / cos a

sinh : Float -> Float
sinh x = (e^x - e^(-x)) / 2

degreesToRadians d = d * pi / 180.0
radiansToDegrees r = r / pi * 180.0
