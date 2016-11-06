module Map exposing
  ( MapConfig
  , Msg
  , Model
  , openStreetMapConfig
  , mapModel
  , view
  , update
  , subscriptions
  )

import Debug
import Html as Html
import Html.Attributes as Attributes
import Html.Events exposing (on, onClick)
import List.Extra as ListE
import Json.Decode as Json
import Mouse

import Geometry exposing (..)
import Projection exposing (..)
import Util exposing (..)

type alias MapConfig =
  { tileUrlPattern : String
  , tileSize : TileSize
  }

type alias Model =
  { config : MapConfig
  , centre : MapPoint
  , zoom : Int
  , renderSize : MapSize
  , drag : Maybe Drag
  }

type alias Drag =
  { start : Mouse.Position
  , current : Mouse.Position
  }

type Msg
  = MapResize MapSize
  | DragStart Mouse.Position
  | DragAt Mouse.Position
  | DragEnd Mouse.Position
  | ZoomIn
  | ZoomOut

openStreetMapConfig : MapConfig
openStreetMapConfig =
  { tileUrlPattern = "http://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png"
  , tileSize = { width = 256, height = 256 }
  }

mapModel : MapConfig -> MapRegion -> Model
mapModel config region =
  let
    (mapCentre, mapZoom) = centreAndZoomFromRegion region
  in
    { config = config
    , centre = mapCentre
    , zoom = mapZoom
    , renderSize = {width=600, height=400}
    , drag = Nothing
    }

centreAndZoomFromRegion : MapRegion -> (MapPoint, Int)
centreAndZoomFromRegion {northWest, southEast} =
  let
    centreX = (northWest.x + southEast.x) / 2
    centreY = (northWest.y + southEast.y) / 2
    zoom = 8 -- TBD
  in
    ({x = centreX, y = centreY}, zoom)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    MapResize size ->
      ({ model | renderSize = size}, Cmd.none)
    DragStart xy ->
      ({ model | drag = Just (Drag xy xy) }, Cmd.none)
    DragAt xy ->
      ({ model | drag = Maybe.map (\{start} -> Drag start xy) model.drag }, Cmd.none)
    DragEnd xy ->
      (applyDrag model, Cmd.none)
    ZoomIn ->
      (zoom model 1, Cmd.none)
    ZoomOut ->
      (zoom model -1, Cmd.none)

zoom : Model -> Int -> Model
zoom model delta =
  let
    latLon = latLonFromMapPoint model.zoom model.centre
    zoom = model.zoom + delta
    centre = mapPointFromLatLon zoom latLon
  in
    { model | centre = centre, zoom = zoom }

applyDrag : Model -> Model
applyDrag model =
  case model.drag of
    Nothing ->
      model
    Just {start, current} ->
      let
        dx = toFloat (start.x - current.x)
        dy = toFloat (start.y - current.y)
        centreDX = dx / toFloat model.config.tileSize.width
        centreDY = dy / toFloat model.config.tileSize.height
      in
        { model
          | centre = translatePoint model.centre centreDX centreDY
          , drag = Nothing
        }

subscriptions : Model -> Sub Msg
subscriptions model =
  case model.drag of
    Nothing ->
      Sub.none

    Just _ ->
      Sub.batch [ Mouse.moves DragAt, Mouse.ups DragEnd ]

getMapSize : Json.Decoder MapSize
getMapSize =
  let
    width = Json.at ["target", "innerWidth"] Json.int
    height = Json.at ["target", "innerHeight"] Json.int
  in
    Json.object2 (\w h -> { width = w, height = h }) width height

view : Model -> Html.Html Msg
view model =
  Html.div
  [ Attributes.style
    [ ("width", "100%")
    , ("height", "100%")
    , ("position", "relative") ]
    , on "resize" (Json.map MapResize getMapSize)
  ]
  [ viewMapLayer model
  , viewControlsLayer model
  ]

viewMapLayer : Model -> Html.Html Msg
viewMapLayer model =
  let
    renderModel = applyDrag model
    tiles' = tiles renderModel
    tileView' = tileView renderModel
  in
    Html.div
      [ on "mousedown" (Json.map DragStart Mouse.position)
      , Attributes.style
        [ ("width", "100%")
        , ("height", "100%")
        , ("position", "relative")
        , ("overflow", "hidden")
        , ("background", "yellow")
        ]
      ]
      (List.map tileView' tiles')

tiles : Model -> List Tile
tiles {centre, config, renderSize} =
  let
    tileSize = config.tileSize
    offsetX = truncate (frac centre.x * toFloat tileSize.width)
    offsetY = truncate (frac centre.y * toFloat tileSize.height)
    centreX = renderSize.width // 2 - offsetX
    centreY = renderSize.height // 2 - offsetY
    beforeX = roundDiv centreX tileSize.width
    beforeY = roundDiv centreY tileSize.height
    afterX = (roundDiv (renderSize.width - centreX) tileSize.width) - 1
    afterY = (roundDiv (renderSize.height - centreY) tileSize.height) - 1
  in
    [-beforeX .. afterX] `ListE.andThen` \x ->
    [-beforeY .. afterY] `ListE.andThen` \y ->
      [ { x = x, y = y } ]

tileView: Model -> Tile -> Html.Html Msg
tileView model pos =
   Html.img
    [ Attributes.style
      [ ("position", "absolute")
      , ("width", px model.config.tileSize.width)
      , ("height", px model.config.tileSize.height)
      , ("transform", translate3d model pos)
      ]
    , Attributes.draggable "false"
    , Attributes.src (tileUrl model pos)
    ]
    []

translate3d : Model -> Tile -> String
translate3d {renderSize, config, centre} pos =
  let
    tileSize = config.tileSize
    fpos = mapPoint toFloat pos
    scale i f = round ((toFloat i) * f)
    x = renderSize.width // 2 + scale tileSize.width (fpos.x - frac centre.x)
    y = renderSize.height // 2 + scale tileSize.height (fpos.y - frac centre.y)
  in
    stringWithSubstitutions "translate3d({x}px, {y}px, 0px)"
      [ ("{x}", toString x)
      , ("{y}", toString y) ]

tileUrl : Model -> Tile -> String
tileUrl {config, centre, zoom} {x, y} =
  let
    tileX = (truncate centre.x) + x
    tileY = (truncate centre.y) + y
  in
    stringWithSubstitutions config.tileUrlPattern
      [ ("{s}", x+y |> subdomain)
      , ("{x}", toString tileX)
      , ("{y}", toString tileY)
      , ("{z}", toString zoom)
      ]

subdomain: Int -> String
subdomain n =
  case n % 3 of
    0 -> "a"
    1 -> "b"
    _ -> "c"

viewControlsLayer : Model -> Html.Html Msg
viewControlsLayer model =
  Html.div
  []
  [ viewControlButton {x=20, y=20} ZoomIn "+"
  , viewControlButton {x=20, y=50} ZoomOut "-"
  ]

viewControlButton : Point Int -> Msg -> String -> Html.Html Msg
viewControlButton {x,y} action label =
  Html.button
  [ Attributes.style
    [ ("position", "absolute")
    , ("left", px x)
    , ("top", px y)
    , ("font-size", "large")
    , ("width", "30px")
    ]
  , onClick action
  ]
  [Html.text label]
