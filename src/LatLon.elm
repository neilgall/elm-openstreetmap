module LatLon exposing
  ( Position
  , Bounds
  , Zoom
  , Degrees
  , boundsCentre
  , toMapPoint
  , fromMapPoint
  , zoomForBounds
  )

import Point
import Size
import Util exposing (frac)

type alias Zoom = Int
type alias Degrees = Float
type alias Radians = Float

type alias Position =
  { latitude : Degrees
  , longitude : Degrees
  }

type alias Bounds =
  { southWest : Position
  , northEast : Position
  }

sec : Radians -> Float
sec a = 1 / cos a

sinh : Radians -> Float
sinh x = (e^x - e^(-x)) / 2

degreesToRadians : Degrees -> Radians
degreesToRadians d = d * pi / 180.0

radiansToDegrees : Radians -> Degrees
radiansToDegrees r = r / pi * 180.0

scaleFromZoom : Zoom -> Float
scaleFromZoom z = toFloat <| 2^z

zoomFromScale : Float -> Zoom
zoomFromScale s = 1 + (floor <| logBase 2 s)

boundsCentre : Bounds -> Position
boundsCentre {southWest, northEast} =
  let
    lat = (southWest.latitude + northEast.latitude) / 2
    lon = (southWest.longitude + northEast.longitude) / 2
  in
    {latitude = lat, longitude = lon}

-- see https://wiki.openstreetmap.org/wiki/Slippy_map_tilenames

toMapPoint : Zoom -> Position -> Point.Map
toMapPoint zoom {latitude, longitude} =
  let
    scale = scaleFromZoom zoom
    x = scale * unscaledTileXFromLongitude longitude
    y = scale * unscaledTileYFromLatitude latitude
  in
    { x = x, y = y }

fromMapPoint : Zoom -> Point.Map -> Position
fromMapPoint zoom {x, y} =
  let
    scale = scaleFromZoom zoom
    lon = longitudeFromUnscaledTileX <| x / scale
    lat = latitudeFromUnscaledTileY <| y / scale
  in
    { latitude = lat, longitude = lon }

-- Tile X position for a longitude in range 0..1 (i.e. zoom not applied)
unscaledTileXFromLongitude : Degrees -> Float
unscaledTileXFromLongitude longitude = (longitude + 180) / 360

-- Tile Y position for a latitude in range 0..1 (i.e. zoom not applied)
unscaledTileYFromLatitude : Degrees -> Float
unscaledTileYFromLatitude latitude =
  let
    latR = degreesToRadians latitude
  in
    (1 - (logBase e (tan latR + sec latR)) / pi) / 2

-- Longitude from unscaled tile X position in range 0..1
longitudeFromUnscaledTileX : Float -> Degrees
longitudeFromUnscaledTileX x = x * 360 - 180

-- Latitude from unscaled tile Y position in range 0..1
latitudeFromUnscaledTileY : Float -> Degrees
latitudeFromUnscaledTileY y =
  radiansToDegrees <| atan <| sinh <| pi * (1 - 2 * y)

-- Map zoom value for a map region and a map size in tiles
zoomForBounds : Bounds -> Size.Map -> Zoom
zoomForBounds {southWest, northEast} {width, height} =
  let
    lonDelta = degreesToRadians <| northEast.longitude - southWest.longitude
    xscale = (toFloat width) / lonDelta
    minY = unscaledTileYFromLatitude northEast.latitude
    maxY = unscaledTileYFromLatitude southWest.latitude
    yscale = (toFloat height) / (maxY - minY)
  in
    zoomFromScale <| min xscale yscale

