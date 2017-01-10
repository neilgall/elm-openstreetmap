import Html exposing (..)
import Html.Attributes exposing (..)
import LatLon
import Map
import Marker

main = Html.program
  { init = init
  , view = view
  , update = Map.update
  , subscriptions = Map.subscriptions
  }

scotland : LatLon.Bounds
scotland = { southWest = LatLon.Position 54.07 -6.67
           , northEast = LatLon.Position 58.84 -1.62
           }
           
edinburgh = { position = LatLon.Position 55.95 -3.2
            , title = "Edinburgh"
            }

init : (Map.Model, Cmd Map.Msg)
init =
  ( Map.mapModel Map.openStreetMapConfig scotland [edinburgh]
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
