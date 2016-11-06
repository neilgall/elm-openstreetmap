import Html exposing (..)
import Html.App as App
import Html.Attributes exposing (..)
import Geometry
import Map

main = App.program
  { init = init
  , view = view
  , update = Map.update
  , subscriptions = Map.subscriptions
  }

scotlandRegion : Geometry.MapRegion
scotlandRegion =
  { northWest = { x = 120, y = 70 }
  , southEast = { x = 130, y = 80 }
  }

init : (Map.Model, Cmd Map.Msg)
init =
  ( Map.mapModel Map.openStreetMapConfig scotlandRegion
  , Cmd.none
  )

view : Map.Model -> Html Map.Msg
view model =
  div []
  [ h1 [] [text "elm-openstreetmap"]
  , p []
    [ text "View "
    , a
      [ href "https://github.com/neilgall/elm-openstreetmap" ]
      [ text "source" ]
    , text " on github"
    ]
  , div
    [ style [ ("width", "600px"), ("height", "400px") ] ]
    [ Map.view model ]
  ]
