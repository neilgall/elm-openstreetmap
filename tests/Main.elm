port module Main exposing (..)

import Test.Runner.Node exposing (run, TestProgram)
import Json.Encode exposing (Value)
import Tests


main : TestProgram
main =
    run emit Tests.all


port emit : ( String, Value ) -> Cmd msg
