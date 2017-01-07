module Intree.Update exposing (..)

import Intree.Model as Model
    exposing
        ( Options
        , Model
        , WheelEvent
        , Coordinate
        , Point
        , Tile
        , Layer
        , tileId
        )
import Mouse
import Task
import List
import Dict exposing (Dict)


type Msg
    = Pan Int Int
    | StartDrag Point
    | StopDrag
    | Zoom WheelEvent
    | LoadTiles
    | ImageLoaded String


subscriptions : Model -> Sub Msg
subscriptions model =
    if model.dragging then
        Sub.batch
            [ Mouse.moves (\{ x, y } -> Pan x y)
            , Mouse.ups (\_ -> StopDrag)
            ]
    else
        Sub.none


loadTiles : Options -> Coordinate -> Dict String Tile
loadTiles options center =
    let
        numHorizontal =
            ceiling <| toFloat options.width / toFloat options.tileSize

        numVertical =
            ceiling <| toFloat options.height / toFloat options.tileSize

        topLng =
            floor <| center.lng + (toFloat options.width / 2.0 / toFloat options.tileSize)

        leftLat =
            floor <| center.lat + (toFloat options.height / 2.0 / toFloat options.tileSize)

        horizontalRange =
            List.range topLng (topLng + numHorizontal)

        verticalRange =
            List.range leftLat (leftLat + numVertical)
    in
        horizontalRange
            |> List.map Tile
            |> List.concatMap (\tileX -> List.map tileX verticalRange)
            |> List.map (\tileXY -> tileXY options.zoomLevel)
            |> List.map (\tileXYZ -> tileXYZ False)
            |> List.map (\tile -> ( tileId tile, tile ))
            |> Dict.fromList


init : Options -> ( Model, Cmd Msg )
init options =
    ( { tiles = loadTiles options options.center
      , prevPosition = Point 0 0
      , dragging = False
      , center = options.center
      , zoomLevel = options.zoomLevel
      , options = options
      }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        StartDrag pos ->
            ( { model | dragging = True, prevPosition = pos }, Cmd.none )

        StopDrag ->
            ( { model | dragging = False }, Cmd.none )

        Pan x y ->
            let
                moveLng =
                    toFloat (model.prevPosition.x - x) / 256.0

                moveLat =
                    toFloat (model.prevPosition.y - y) / 256.0

                newModel =
                    { model
                        | prevPosition = Point x y
                        , center =
                            { lng = model.center.lng + moveLng
                            , lat = model.center.lat + moveLat
                            }
                    }
            in
                ( newModel, Task.perform (\_ -> LoadTiles) <| Task.succeed () )

        Zoom event ->
            ( { model | zoomLevel = model.zoomLevel + round event.deltaY }, Cmd.none )

        LoadTiles ->
            let
                newTiles =
                    loadTiles model.options model.center
            in
                ( { model | tiles = Dict.union model.tiles newTiles }, Cmd.none )

        ImageLoaded loadedTileId ->
            let
                load tile =
                    { tile | loaded = True }

                maybeLoad =
                    Maybe.map load

                newTiles =
                    Dict.update loadedTileId maybeLoad model.tiles
            in
                ( { model | tiles = newTiles }, Cmd.none )
