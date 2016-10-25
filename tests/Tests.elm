module Tests exposing (all)
import Test exposing (..)
import Expect

all : Test
all =
  describe "elm-openstreetmap"
    [ tiles
    ]

tiles : Test
tiles =
  describe "tiles"
    [ test "should expand renderModel position range into all tiles" <|
      \() ->
        let
          rModel = { across = 5, down = 5, xmin = 2, ymin = 6, xmax = 6, ymax = 10 }
        in
          tiles rModel
          |> Expect.equal [
            { x=2, y=6 }, { x=3, y=6 }, { x=4, y=6 }, { x=5, y=6 },
            { x=2, y=7 }, { x=3, y=7 }, { x=4, y=7 }, { x=5, y=7 },
            { x=2, y=8 }, { x=3, y=8 }, { x=4, y=8 }, { x=5, y=8 },
            { x=2, y=9 }, { x=3, y=9 }, { x=4, y=9 }, { x=5, y=9 },
            { x=2, y=10 }, { x=3, y=10 }, { x=4, y=10 }, { x=5, y=10 },
          ]
