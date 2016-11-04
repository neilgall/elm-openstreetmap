module ProjectionTests exposing (..)

import Debug
import Test exposing (..)
import Fuzz exposing (floatRange, intRange, map2)
import Expect
import String

import Projection exposing (..)
import TestUtil exposing (..)

all : Test
all =
    describe "Projection tests"
      [ mapPointLatLonIsomorphism
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

randomLatitude = floatRange -85.05 85.05
randomLongitude = floatRange -180 179.99
randomLatLon = Fuzz.map2 LatLon randomLatitude randomLongitude
randomZoom = intRange 5 18
