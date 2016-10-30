module Map exposing
  ( Config
  , Model
  , Msg
  , defaultModel
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

type alias Config =
  { tileUrlPattern : String }

type alias Model =
  { config : Config
  , centre : Point
  , offset : Point
  , zoom : Int
  , renderSize : Size
  , tileSize : Size
  , drag : Maybe Drag
  }

type alias Drag =
  { start : Point
  , current : Point
  }

type Msg
  = DragStart Point
  | DragAt Point
  | DragEnd Point
  | ZoomIn
  | ZoomOut

defaultModel =
  { config = { tileUrlPattern = "http://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png" }
  , centre = { x = 123, y = 79 }
  , zoom = 8
  , offset = { x = 0, y = 0 }
  , renderSize = { width = 400, height = 400 }
  , tileSize = { width = 256, height = 256 }
  , drag = Nothing
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
    ZoomIn ->
      (zoom model 1, Cmd.none)
    ZoomOut ->
      (zoom model -1, Cmd.none)

zoom : Model -> Int -> Model
zoom model delta =
  let
    latLon = latLonFromPoint model.zoom model.centre
    zoom = model.zoom + delta
    centre = pointFromLatLon zoom latLon
  in
    { model | centre = centre, zoom = zoom }

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
        { model
          | centre = translatePoint model.centre -centreDX -centreDY
          , offset = translatePoint model.offset offsetDX offsetDY
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
  Html.div []
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
        [ ("width", px model.renderSize.width)
        , ("height", px model.renderSize.height)
        , ("position", "relative")
        , ("overflow", "hidden")
        ]
      ]
      (List.map tileView' tiles')

tiles : Model -> List Point
tiles model =
  (relativeTileRange model .x .width) `ListE.andThen` \x ->
  (relativeTileRange model .y .height) `ListE.andThen` \y ->
  [ translatePoint model.centre x y ]

relativeTileRange : Model -> (Point -> Int) -> (Size -> Int) -> List Int
relativeTileRange model axis size =
  let
    tilesForSize s = (s + size model.tileSize - 1) // size model.tileSize
    centre = centreTileCoord model axis size
    before = tilesForSize centre
    after = tilesForSize (size model.renderSize - centre) - 1
  in
    [-before .. after]

centreTileCoord : Model -> (Point -> Int) -> (Size -> Int) -> Int
centreTileCoord {renderSize, tileSize, offset} axis size =
  (size renderSize - size tileSize) // 2 + axis offset

tileView: Model -> Point -> Html.Html Msg
tileView model pos =
   Html.img
    [ Attributes.style
      [ ("position", "absolute")
      , ("width", px model.tileSize.width)
      , ("height", px model.tileSize.height)
      , ("transform", translate3d model pos)
      ]
    , Attributes.draggable "false"
    , Attributes.src (tileUrl model pos)
    ]
    []

translate3d: Model -> Point -> String
translate3d model pos =
  let
    coord axis size = (axis pos - axis model.centre) * size model.tileSize
    x = (centreTileCoord model .x .width) + coord .x .width
    y = (centreTileCoord model .y .height) + coord .y .height
  in
    stringWithSubstitutions "translate3d({x}px, {y}px, 0px)"
      [ ("{x}", toString x)
      , ("{y}", toString y) ]

tileUrl: Model -> Point -> String
tileUrl {config, zoom} {x, y} =
  stringWithSubstitutions config.tileUrlPattern
    [ ("{s}", x+y |> subdomain)
    , ("{x}", toString x)
    , ("{y}", toString y)
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
  , viewControlButton {x=20, y=40} ZoomOut "-"
  ]

viewControlButton : Point -> Msg -> String -> Html.Html Msg
viewControlButton {x,y} action label =
  Html.button
  [ Attributes.style
    [ ("position", "absolute")
    , ("left", px x)
    , ("top", px y)
    , ("width", "20px")
    ]
  , onClick action
  ]
  [Html.text label]
