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
import Html
import Html.Attributes as Attributes
import Html.Events exposing (on)
import Json.Decode as Json
import Mouse

import Controls
import Geometry exposing (..)
import Message exposing (..)
import Projection exposing (..)
import Util exposing (..)

type alias Msg = Message.Msg

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

openStreetMapConfig : MapConfig
openStreetMapConfig =
  { tileUrlPattern = "http://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png"
  , tileSize = { width = 256, height = 256 }
  }

mapModel : MapConfig -> LatLonBounds -> Model
mapModel config bounds =
  let
    renderSize = {width=600, height=400}
    tilesX = renderSize.width // config.tileSize.width + 1
    tilesY = renderSize.height // config.tileSize.height + 1
    mapSize = {width = tilesX, height = tilesY}
    (mapCentre, mapZoom) = centreAndZoomFromRegion bounds mapSize
  in
    { config = config
    , centre = mapCentre
    , zoom = mapZoom
    , renderSize = renderSize
    , drag = Nothing
    }

centreAndZoomFromRegion : LatLonBounds -> MapSize -> (MapPoint, Int)
centreAndZoomFromRegion bounds mapSize =
  let
    zoom = zoomForLatLonRange bounds mapSize
    centre = mapPointFromLatLon zoom (latlonBoundsCentre bounds)
  in
    (centre, zoom)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case (Debug.log "msg" msg) of
    MapResize size ->
      ({ model | renderSize = size}, Cmd.none)
    DragStart xy ->
      ({ model | drag = Just (Drag xy xy) }, Cmd.none)
    DragAt xy ->
      ({ model | drag = Maybe.map (\{start} -> Drag start xy) model.drag }, Cmd.none)
    DragEnd xy ->
      (applyDrag model, Cmd.none)
    ZoomIn ->
      (adjustZoom model 1, Cmd.none)
    ZoomOut ->
      (adjustZoom model -1, Cmd.none)

adjustZoom : Model -> Int -> Model
adjustZoom model delta =
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
    width = Json.at ["target", "offsetWidth"] Json.float
    height = Json.at ["target", "offsetHeight"] Json.float
  in
    Json.map2 (\w h -> { width = round w, height = round h }) width height

view : Model -> Html.Html Msg
view model =
  Html.div
  [ Attributes.style
    [ ("width", "100%")
    , ("height", "100%")
    , ("position", "relative")
    ]
  , on "load" (Json.map MapResize getMapSize)
  , on "resize" (Json.map MapResize getMapSize)
  ]
  [ viewMapLayer model
  , Controls.view
  ]

viewMapLayer : Model -> Html.Html Msg
viewMapLayer model =
  let
    renderModel = applyDrag model
  in
    Html.div
      [ on "mousedown" (Json.map DragStart Mouse.position)
      , Attributes.style
        [ ("width", "100%")
        , ("height", "100%")
        , ("position", "relative")
        , ("overflow", "hidden")
        ]
      ]
      (List.map (tileView renderModel) (tiles renderModel))

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
    xRange = List.range -beforeX afterX
    yRange = List.range -beforeY afterY
  in
    List.concatMap (\x -> List.map (\y -> { x=x, y=y }) yRange) xRange

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
