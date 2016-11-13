import Html exposing (..)
import Html.App as App
import Html.Attributes exposing (..)
import Projection exposing (LatLon)
import Map

main = App.program
  { init = init
  , view = view
  , update = Map.update
  , subscriptions = Map.subscriptions
  }

scotlandSouthWest : LatLon
scotlandSouthWest = LatLon 54.07 -6.67

scotlandNorthEast : LatLon
scotlandNorthEast = LatLon 58.84 -1.62

init : (Map.Model, Cmd Map.Msg)
init =
  ( Map.mapModel Map.openStreetMapConfig scotlandSouthWest scotlandNorthEast
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
