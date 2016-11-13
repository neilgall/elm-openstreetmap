module ProjectionTests exposing (..)

import Debug
import Test exposing (..)
import Fuzz exposing (..)
import Expect

import Geometry exposing (..)
import Projection exposing (..)
import TestUtil exposing (..)

all : Test
all =
    describe "Projection tests"
      [ mapPointLatLonIsomorphism
      , zoomForLatLonRange_keepsRangeVisible
      ]

mapPointLatLonIsomorphism =
  fuzz2 randomLatLon randomZoom "Lat,Lon and MapPoint are isomorphic" <|
    \latlon zoom ->
      let
        mapPoint = mapPointFromLatLon zoom latlon
        latlon' = latLonFromMapPoint zoom mapPoint
    in
      expectAll
        [ expectEqualWithin 1e-8 latlon.latitude latlon'.latitude
        , expectEqualWithin 1e-8 latlon.longitude latlon'.longitude
        ]

zoomForLatLonRange_keepsRangeVisible =
  fuzz3 randomLatLonBounds randomTileCount randomTileCount
    "LatLon range is visible when calculating zoom" <|
    \bounds w h ->
      let
        zoom = zoomForLatLonRange bounds (Size w h)
        swMapPoint = mapPointFromLatLon zoom bounds.southWest
        neMapPoint = mapPointFromLatLon zoom bounds.northEast
        centreMapPoint = mapPointFromLatLon zoom <| latlonBoundsCentre bounds
        halfWidth = (toFloat w) / 2
        halfHeight = (toFloat h) / 2
      in
        expectAll
          [ Expect.greaterThan (centreMapPoint.x - halfWidth) swMapPoint.x
          , Expect.greaterThan (centreMapPoint.y - halfWidth) swMapPoint.y
          , Expect.lessThan (centreMapPoint.x + halfWidth) neMapPoint.x
          , Expect.lessThan (centreMapPoint.y + halfWidth) neMapPoint.y
          ]

minimumLatitude = -85.05
maximumLatitude = 85.05
minimumLongitude = -180
maximumLongitude = 179.999

randomLatitude = floatRange minimumLatitude maximumLatitude
randomLongitude = floatRange minimumLongitude maximumLongitude
randomLatLon = map2 LatLon randomLatitude randomLongitude
randomZoom = intRange 5 18
randomTileCount = intRange 2 10

randomOrderedPair : Float -> Float -> Fuzzer (Float, Float)
randomOrderedPair minimum maximum =
  andThen (\lower ->
     map2 (,) (constant lower) (floatRange (lower + 1e-3) maximum))
     (floatRange minimum (maximum - 1e-3))

randomLatitudePair : Fuzzer (Degrees, Degrees)
randomLatitudePair = randomOrderedPair minimumLatitude maximumLatitude

randomLongitudePair : Fuzzer (Degrees, Degrees)
randomLongitudePair = randomOrderedPair minimumLongitude maximumLongitude

randomLatLonBounds : Fuzzer LatLonBounds
randomLatLonBounds =
  let
    boundsFrom lats lons =
      { southWest = LatLon (fst lats) (fst lons)
      , northEast = LatLon (snd lats) (snd lons)
      }
  in
    map2 boundsFrom randomLatitudePair randomLongitudePair
