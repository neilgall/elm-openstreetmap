module Tests exposing (..)

import Test exposing (..)
import Expect
import String

import MapTests

all : Test
all =
    describe "All tests"
      [ MapTests.all
      ]
