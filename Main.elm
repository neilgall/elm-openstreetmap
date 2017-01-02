import Html exposing (..)
import Html.Attributes exposing (..)
import Projection exposing (LatLon, LatLonBounds)
import Map

main = Html.program
  { init = init
  , view = view
  , update = Map.update
  , subscriptions = Map.subscriptions
  }

scotland : LatLonBounds
scotland = { southWest = LatLon 54.07 -6.67
           , northEast = LatLon 58.84 -1.62
           }

init : (Map.Model, Cmd Map.Msg)
init =
  ( Map.mapModel Map.openStreetMapConfig scotland
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
