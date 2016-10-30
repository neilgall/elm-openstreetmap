import Html.App as App
import Map

main = App.program
  { init = init
  , view = Map.view
  , update = Map.update
  , subscriptions = Map.subscriptions
  }

init : (Map.Model, Cmd Map.Msg)
init = (Map.defaultModel, Cmd.none)
