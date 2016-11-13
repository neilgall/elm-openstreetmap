module Projection exposing
  ( LatLon
  , LatLonSpan
  , Zoom
  , Degrees
  , mapPointFromLatLon
  , latLonFromMapPoint
  , zoomForLatLonRange
  )

import Geometry exposing (..)

type alias Zoom = Int
type alias Metres = Float
type alias Degrees = Float
type alias Radians = Float

type alias LatLon =
  { latitude : Degrees
  , longitude : Degrees
  }

type alias LatLonSpan =
  { latitudeSpan : Degrees
  , longitudeSpan : Degrees
  }

sec : Radians -> Float
sec a = 1 / cos a

sinh : Radians -> Float
sinh x = (e^x - e^(-x)) / 2

degreesToRadians : Degrees -> Radians
degreesToRadians d = d * pi / 180.0

radiansToDegrees : Radians -> Degrees
radiansToDegrees r = r / pi * 180.0

-- see http://www.movable-type.co.uk/scripts/latlong.html

radiusOfEarth : Metres
radiusOfEarth = 6371e3

distance : LatLon -> LatLon -> Metres
distance p1 p2 =
  let
    lat1 = degreesToRadians p1.latitude
    lat2 = degreesToRadians p2.latitude
    lon1 = degreesToRadians p1.longitude
    lon2 = degreesToRadians p2.longitude
    latDelta = lat2 - lat1
    lonDelta = lon2 - lon1
    sin2 x = (sin x) ^ 2
    a = (sin2 <| latDelta / 2) + cos lat1 * cos lat2 * (sin2 <| lonDelta / 2)
    c = 2 * (atan2 (sqrt a) (sqrt (1 - a)))
  in
    radiusOfEarth * c

-- see https://wiki.openstreetmap.org/wiki/Slippy_map_tilenames

mapPointFromLatLon : Zoom -> LatLon -> MapPoint
mapPointFromLatLon zoom {latitude, longitude} =
  let
    scale = 2^zoom |> toFloat
    x = scale * tileXFromLongitude longitude
    y = scale * tileYFromLatitude latitude
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

tileXFromLongitude : Degrees -> Float
tileXFromLongitude longitude =
  (longitude + 180) / 360

tileYFromLatitude : Degrees -> Float
tileYFromLatitude latitude =
  let
    lat' = degreesToRadians latitude
  in
    (1 - (logBase e (tan lat' + sec lat')) / pi) / 2

-- Calculate map zoom value for a lat,lon range and a map size in tiles
zoomForLatLonRange : LatLon -> LatLon -> MapSize -> Zoom
zoomForLatLonRange southWest northEast {width, height} =
  let
    latDelta = degreesToRadians <| northEast.latitude - southWest.latitude
    lonDelta = degreesToRadians <| northEast.longitude - southWest.longitude
    minY = tileYFromLatitude northEast.latitude
    maxY = tileYFromLatitude southWest.latitude
    yscale = (toFloat height) / (maxY - minY)
    xscale = (toFloat width) / lonDelta
  in
    1 + (floor <| logBase 2 <| min xscale yscale)
