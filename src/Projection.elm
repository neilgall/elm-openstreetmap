module Projection exposing
  ( LatLon
  , Zoom
  , mapPointFromLatLon
  , latLonFromMapPoint
  )

import Geometry exposing (..)

type alias LatLon =
  { latitude : Float
  , longitude : Float
  }

type alias Zoom = Int

-- see https://wiki.openstreetmap.org/wiki/Slippy_map_tilenames

mapPointFromLatLon : Zoom -> LatLon -> MapPoint
mapPointFromLatLon zoom {latitude, longitude} =
  let
    scale = 2^zoom |> toFloat
    lat' = degreesToRadians latitude
    x = scale * (longitude + 180) / 360
    y = scale * (1 - (logBase e (tan lat' + sec lat')) / pi) / 2
  in
    { x = x, y = y }

latLonFromMapPoint : Zoom -> MapPoint -> LatLon
latLonFromMapPoint zoom {x, y} =
  let
    scale = 2^zoom |> toFloat
    lon = x / scale * 360 - 180
    lat' = sinh (pi * (1 - 2 * y / scale)) |> atan
    lat = radiansToDegrees lat'
  in
    { latitude = lat, longitude = lon }

sec : Float -> Float
sec a = 1 / cos a

sinh : Float -> Float
sinh x = (e^x - e^(-x)) / 2

degreesToRadians d = d * pi / 180.0
radiansToDegrees r = r / pi * 180.0
