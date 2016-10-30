module Map exposing
  ( Size
  , Config
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
import Html.Events exposing (on)
import List.Extra as ListE
import Json.Decode as Json
import Mouse exposing (Position)

import Util exposing (..)

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
  , zoom : Int
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

defaultModel =
  { config = { tileUrlPattern = "http://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png" }
  , centre = { x = 123, y = 79 }
  , offset = { x = 0, y = 0 }
  , zoom = 8
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

tiles : Model -> List Position
tiles model =
  (relativeTileRange model .x .width) `ListE.andThen` \x ->
  (relativeTileRange model .y .height) `ListE.andThen` \y ->
    [{ x = model.centre.x + x, y = model.centre.y + y }]

relativeTileRange : Model -> (Position -> Int) -> (Size -> Int) -> List Int
relativeTileRange model axis size =
  let
    centre = (size model.renderSize - size model.tileSize) // 2 + axis model.offset
    tilesForSize s = (s + size model.tileSize - 1) // size model.tileSize
    before = tilesForSize centre
    after = tilesForSize (size model.renderSize - centre) - 1
  in
    [-before .. after]

tileView: Model -> Position -> Html.Html Msg
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

translate3d: Model -> Position -> String
translate3d model pos =
  let
    coord axis size =
      ((size model.renderSize - size model.tileSize) // 2)
      + (axis pos - axis model.centre) * size model.tileSize
      + axis model.offset

    x = coord .x .width
    y = coord .y .height
  in
    stringWithSubstitutions "translate3d({x}px, {y}px, 0px)"
      [ ("{x}", toString x)
      , ("{y}", toString y) ]

tileUrl: Model -> Position -> String
tileUrl model pos =
  stringWithSubstitutions model.config.tileUrlPattern
    [ ("{s}", subdomain pos)
    , ("{z}", toString model.zoom)
    , ("{y}", toString pos.y)
    , ("{x}", toString pos.x)
    ]

subdomain: Position -> String
subdomain pos =
  case (pos.x + pos.y) % 3 of
    0 -> "a"
    1 -> "b"
    _ -> "c"
