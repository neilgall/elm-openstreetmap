module Tests exposing (..)

import Test exposing (..)
import Expect
import String

import MapTests
import ProjectionTests

all : Test
all =
    describe "All tests"
      [ MapTests.all
      , ProjectionTests.all
      ]
