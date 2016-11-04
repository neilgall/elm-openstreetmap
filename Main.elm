import Html exposing (..)
import Html.App as App
import Html.Attributes exposing (..)
import Map

main = App.program
  { init = init
  , view = view
  , update = Map.update
  , subscriptions = Map.subscriptions
  }

init : (Map.Model, Cmd Map.Msg)
init = (Map.defaultModel, Cmd.none)

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
  , Map.view model
  ]
