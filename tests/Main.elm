port module Main exposing (..)

import Test.Runner.Node exposing (run)
import Json.Encode exposing (Value)
import Tests


main : Program Value
main =
    run emit Tests.all


port emit : ( String, Value ) -> Cmd msg
