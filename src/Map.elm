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
import LatLon
import Marker
import Message exposing (..)
import Point
import Projection
import Render
import Size
import Util exposing (frac, roundDiv, stringWithSubstitutions)

type alias Msg = Message.Msg

type alias MapConfig =
  { tileUrlPattern : String
  , tileSize : Size.Tile
  }

type alias Model =
  { config : MapConfig
  , centre : Point.Map
  , zoom : Int
  , renderSize : Size.Map
  , markers : List Marker.Marker
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

mapModel : MapConfig -> LatLon.Bounds -> List Marker.Marker -> Model
mapModel config bounds markers =
  let
    renderSize = {width=600, height=400}
    tilesX = renderSize.width // config.tileSize.width + 1
    tilesY = renderSize.height // config.tileSize.height + 1
    mapSize = {width = tilesX, height = tilesY}
    (mapCentre, mapZoom) = centreAndZoomFromBoundsAndTiles bounds mapSize
  in
    { config = config
    , centre = mapCentre
    , zoom = mapZoom
    , renderSize = renderSize
    , markers = markers
    , drag = Nothing
    }

centreAndZoomFromBoundsAndTiles : LatLon.Bounds -> Size.Map -> (Point.Map, Int)
centreAndZoomFromBoundsAndTiles bounds mapSize =
  let
    zoom = LatLon.zoomForBounds bounds mapSize
    centre = LatLon.toMapPoint zoom (LatLon.boundsCentre bounds)
  in
    (centre, zoom)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case (Debug.log "msg" msg) of
    MapResize size ->
      { model | renderSize = size } ! []
    DragStart xy ->
      { model | drag = Just (Drag xy xy) } ! []
    DragAt xy ->
      { model | drag = Maybe.map (\{start} -> Drag start xy) model.drag } ! []
    DragEnd xy ->
      applyDrag model ! []
    ZoomIn ->
      adjustZoom model 1 ! []
    ZoomOut ->
      adjustZoom model -1 ! []

adjustZoom : Model -> Int -> Model
adjustZoom model delta =
  let
    latLon = LatLon.fromMapPoint model.zoom model.centre
    zoom = model.zoom + delta
    centre = LatLon.toMapPoint zoom latLon
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
          | centre = Point.translate model.centre centreDX centreDY
          , drag = Nothing
        }

subscriptions : Model -> Sub Msg
subscriptions model =
  case model.drag of
    Nothing ->
      Sub.none

    Just _ ->
      Sub.batch [ Mouse.moves DragAt, Mouse.ups DragEnd ]

getMapSize : Json.Decoder Size.Map
getMapSize =
  let
    width = Json.at ["target", "offsetWidth"] Json.float
    height = Json.at ["target", "offsetHeight"] Json.float
  in
    Json.map2 (\w h -> { width = round w, height = round h }) width height

view : Model -> Html.Html Msg
view model =
  let
    renderModel = applyDrag model
  in
    Html.div
    [ Attributes.style
      [ ("width", "100%")
      , ("height", "100%")
      , ("position", "relative")
      ]
    , on "load" (Json.map MapResize getMapSize)
    , on "resize" (Json.map MapResize getMapSize)
    ]
    [ viewMapLayer renderModel
    , viewMarkerLayer renderModel
    , Controls.view
    ]

viewMapLayer : Model -> Html.Html Msg
viewMapLayer model =
  let
    projection = projectionForModel model
  in
    Html.div
      [ on "mousedown" (Json.map DragStart Mouse.position)
      , Render.fullSize
      ]
      (List.map (tileView projection model) (tiles model))

viewMarkerLayer : Model -> Html.Html Msg
viewMarkerLayer model =
  let
    relativeTo c p = { x = c.x - p.x, y = c.y - p.y }
    projection = LatLon.toMapPoint model.zoom >> relativeTo model.centre >> projectionForModel model
  in
    Html.div
      [ Render.fullSize ]
      (List.map (Marker.view projection) model.markers)

tiles : Model -> List Point.Tile
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

tileView: Projection.Projection -> Model -> Point.Tile -> Html.Html Msg
tileView projection model pos =
   Html.img
    [ Attributes.style
      [ ("position", "absolute")
      , ("width", Render.px model.config.tileSize.width)
      , ("height", Render.px model.config.tileSize.height)
      , ("transform", Point.map toFloat pos |> projection |> Render.translate3d)
      ]
    , Attributes.draggable "false"
    , Attributes.src (tileUrl model pos)
    ]
    []

projectionForModel : Model -> Projection.Projection
projectionForModel {renderSize, config, centre} =
  Projection.project renderSize config.tileSize centre

tileUrl : Model -> Point.Tile -> String
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
