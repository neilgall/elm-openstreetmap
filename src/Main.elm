import Html as Html
import Html.Attributes as Attributes
import Html.App as App
import Html.Events
import List.Extra as ListE

main = App.beginnerProgram {
    model = model,
    view = view,
    update = update
  }

type alias Position =
  { x: Int
  , y: Int
  }

type alias Size =
  { width: Int
  , height: Int
  }

type alias Model =
  { centre: Position
  , renderSize: Size
  , tileSize: Size
  }

model: Model
model =
  { centre = { x = 123, y = 79 }
  , renderSize = { width = 600, height = 600 }
  , tileSize = { width = 256, height = 256 }
  }

type alias RenderModel =
  { model: Model
  , across: Int
  , down: Int
  , xmin: Int
  , ymin: Int
  , xmax: Int
  , ymax: Int
  }

renderModel: Model -> RenderModel
renderModel model =
  let
    across = model.renderSize.width // model.tileSize.width + 1
    down = model.renderSize.height // model.tileSize.height + 1
  in
    { model = model
    , across = across
    , down = down
    , xmin = model.centre.x - across // 2
    , xmax = model.centre.x + across // 2
    , ymin = model.centre.y - down // 2
    , ymax = model.centre.y + down // 2
    }

tiles: RenderModel -> List Position
tiles rmodel =
  [rmodel.xmin .. rmodel.xmax] `ListE.andThen` \x ->
  [rmodel.ymin .. rmodel.ymax] `ListE.andThen` \y ->
    [{ x = x, y = y}]

type Msg = None

update: Msg -> Model -> Model
update _ model = model

view: Model -> Html.Html Msg
view model =
  let
    rModel = renderModel model
    tilePositions = tiles rModel
  in
    Html.div [
      Attributes.style [
        ("width", (toString model.renderSize.width) ++ "px"),
        ("height", (toString model.renderSize.height) ++ "px")
        ]
      ]
      (List.map (tileView rModel) tilePositions)

tileView: RenderModel -> Position -> Html.Html Msg
tileView rModel pos =
  Html.img [
    Attributes.style [
      ("position", "absolute"),
      ("width", toString model.tileSize.width ++ "px"),
      ("height", toString model.tileSize.height ++ "px"),
      ("transform", translate3d rModel pos )
    ],
    Attributes.src (tileUrl rModel pos)
  ] []

translate3d: RenderModel -> Position -> String
translate3d rModel pos =
  let
    x = (pos.x - rModel.xmin) * rModel.model.tileSize.width
    y = (pos.y - rModel.ymin) * rModel.model.tileSize.height
  in
    "translate3d("
    ++ (toString x)
    ++ "px, "
    ++ (toString y)
    ++ "px, 0px)"

tileUrl: RenderModel -> Position -> String
tileUrl rModel pos =
  "http://"
  ++ (subdomain pos)
  ++ ".tile.openstreetmap.org/8/"
  ++ (toString pos.x)
  ++ "/"
  ++ (toString pos.y)
  ++ ".png"

subdomain: Position -> String
subdomain pos =
  case (pos.x + pos.y) % 3 of
    0 -> "a"
    1 -> "b"
    _ -> "c"
