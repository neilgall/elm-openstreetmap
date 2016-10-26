import Debug
import Html as Html
import Html.Attributes as Attributes
import Html.App as App
import Html.Events exposing (on)
import List.Extra as ListE
import Json.Decode as Json
import Mouse exposing (Position)
import String.Extra as StringE

main = App.program
  { init = init
  , view = view
  , update = update
  , subscriptions = subscriptions
  }

type alias Size =
  { width : Int
  , height : Int
  }

type alias Config =
  { tileUrlPattern : String }

type alias Model =
  { config : Config
  , centre : Position
  , offset : Position
  , renderSize : Size
  , tileSize : Size
  , drag : Maybe Drag
  }

type alias Drag =
  { start : Position
  , current : Position
  }

type Msg
  = DragStart Position
  | DragAt Position
  | DragEnd Position

init : (Model, Cmd Msg)
init = (defaultModel, Cmd.none)

defaultModel =
  { config = { tileUrlPattern = "http://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png" }
  , centre = { x = 123, y = 79 }
  , offset = { x = 0, y = 0 }
  , renderSize = { width = 600, height = 600 }
  , tileSize = { width = 256, height = 256 }
  , drag = Nothing
  }

type alias RenderModel =
  { model : Model
  , zoom : Int
  , xmin : Int
  , ymin : Int
  , xmax : Int
  , ymax : Int
  }

renderModel : Model -> RenderModel
renderModel model =
  let
    columns = model.renderSize.width // model.tileSize.width + 1
    rows = model.renderSize.height // model.tileSize.height + 1
  in
    { model = model
    , zoom = 8
    , xmin = model.centre.x - columns // 2
    , xmax = model.centre.x + columns - (columns // 2)
    , ymin = model.centre.y - rows // 2
    , ymax = model.centre.y + rows - (rows // 2)
    }

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    DragStart xy ->
      ({ model | drag = Just (Drag xy xy) }, Cmd.none)
    DragAt xy ->
      ({ model | drag = Maybe.map (\{start} -> Drag start xy) model.drag }, Cmd.none)
    DragEnd xy ->
      (applyDrag model, Cmd.none)

applyDrag : Model -> Model
applyDrag model =
  case model.drag of
    Nothing ->
      model
    Just {start, current} ->
      let
        dx = current.x - start.x
        dy = current.y - start.y
        centreDX = (model.offset.x + dx) // model.tileSize.width
        centreDY = (model.offset.y + dy) // model.tileSize.height
        offsetDX = dx - centreDX * model.tileSize.height
        offsetDY = dy - centreDY * model.tileSize.height
      in
        Debug.log "drag" { model
          | centre = { x = model.centre.x - centreDX, y = model.centre.y - centreDY }
          , offset = { x = model.offset.x + offsetDX, y = model.offset.y + offsetDY }
          , drag = Nothing
        }

subscriptions : Model -> Sub Msg
subscriptions model =
  case model.drag of
    Nothing ->
      Sub.none

    Just _ ->
      Sub.batch [ Mouse.moves DragAt, Mouse.ups DragEnd ]

view : Model -> Html.Html Msg
view model =
  let
    rModel = renderModel <| applyDrag model
    tilePositions = tiles rModel
  in
    Html.div
      [ on "mousedown" (Json.map DragStart Mouse.position)
      , Attributes.style
        [ ("width", (toString model.renderSize.width) ++ "px")
        , ("height", (toString model.renderSize.height) ++ "px")
        ]
      ]
      (List.map (tileView rModel) tilePositions)

tiles : RenderModel -> List Position
tiles rmodel =
  [rmodel.xmin .. rmodel.xmax] `ListE.andThen` \x ->
  [rmodel.ymin .. rmodel.ymax] `ListE.andThen` \y ->
    [{ x = x, y = y}]

tileView: RenderModel -> Position -> Html.Html Msg
tileView rModel pos =
  Html.img
    [ Attributes.style
      [ ("position", "absolute")
      , ("width", px rModel.model.tileSize.width)
      , ("height", px rModel.model.tileSize.height)
      , ("transform", translate3d rModel pos)
      ]
    , Attributes.draggable "false"
    , Attributes.src (tileUrl rModel pos)
    ]
    []

stringWithSubstitutions : String -> List (String, String) -> String
stringWithSubstitutions str subs =
  let replace (a, b) = StringE.replace a b in
  List.foldr replace str subs

px : Int -> String
px n = toString n ++ "px"

translate3d: RenderModel -> Position -> String
translate3d rModel pos =
  let
    x = (pos.x - rModel.xmin) * rModel.model.tileSize.width + rModel.model.offset.x
    y = (pos.y - rModel.ymin) * rModel.model.tileSize.height + rModel.model.offset.y
  in
    stringWithSubstitutions "translate3d({x}px, {y}px, 0px)"
      [ ("{x}", toString x)
      , ("{y}", toString y) ]

tileUrl: RenderModel -> Position -> String
tileUrl rModel pos =
  stringWithSubstitutions rModel.model.config.tileUrlPattern
    [ ("{s}", subdomain pos)
    , ("{z}", toString rModel.zoom)
    , ("{y}", toString pos.y)
    , ("{x}", toString pos.x)
    ]

subdomain: Position -> String
subdomain pos =
  case (pos.x + pos.y) % 3 of
    0 -> "a"
    1 -> "b"
    _ -> "c"
