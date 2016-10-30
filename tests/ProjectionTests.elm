module ProjectionTests exposing (..)

import Debug
import Test exposing (..)
import Fuzz exposing (floatRange, intRange, map2)
import Expect
import String

import Projection exposing (..)

all : Test
all =
    describe "Projection tests"
      [ pointWithinRange
      ]

pointWithinRange =
  fuzz2 randomLatLon randomZoom "Tile X,Y is within range" <|
    \latlon zoom ->
      let
        {x, y} = pointFromLatLon zoom latlon
      in
        Expect.true ("tile " ++ toString x ++ "," ++ toString y)
            (0 <= x && x < 2^zoom
          && 0 <= y && y < 2^zoom)

randomLatitude = floatRange -85.05 85.05
randomLongitude = floatRange -180 179.99
randomLatLon = Fuzz.map2 LatLon randomLatitude randomLongitude
randomZoom = intRange 5 18
