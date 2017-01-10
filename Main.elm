import Html exposing (..)
import Html.Attributes exposing (..)
import LatLon
import Map
import Marker

type Msg = MapMsg Map.Msg

main : Program Never Map.Model Msg
main = Html.program
  { init = init
  , view = view
  , update = update
  , subscriptions = subscriptions
  }

scotland : LatLon.Bounds
scotland = { southWest = LatLon.Position 54.07 -6.67
           , northEast = LatLon.Position 58.84 -1.62
           }
           
edinburgh = { position = LatLon.Position 55.95 -3.2
            , title = "Edinburgh"
            }

init : (Map.Model, Cmd Msg)
init =
  ( Map.mapModel Map.openStreetMapConfig scotland [edinburgh]
  , Cmd.none
  )

view : Map.Model -> Html Msg
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
    [ Html.map MapMsg (Map.view model) ]
  ]

update : Msg -> Map.Model -> (Map.Model, Cmd Msg)
update msg model =
  case msg of
    MapMsg mapMsg ->
      let (newModel, cmd) = Map.update mapMsg model
      in (newModel, Cmd.map MapMsg cmd)

subscriptions : Map.Model -> Sub Msg
subscriptions model =
  Sub.map MapMsg (Map.subscriptions model)
