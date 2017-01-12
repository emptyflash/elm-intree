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
        , TileId
        , TileMap
        , tileId
        )
import Mouse
import Task exposing (Task)
import List
import Task
import List.Extra as List
import Dict exposing (Dict)
import WebGL.Texture as Texture exposing (Texture, Error, defaultOptions, nearest, nearestMipmapNearest)


type Msg
    = Pan Int Int
    | StartDrag Point
    | StopDrag
    | Zoom WheelEvent
    | LoadTiles
    | TextureLoad String Texture
    | TextureError Error


subscriptions : Model -> Sub Msg
subscriptions model =
    if model.dragging then
        Sub.batch
            [ Mouse.moves (\{ x, y } -> Pan x y)
            , Mouse.ups (\_ -> StopDrag)
            ]
    else
        Sub.none


loadTiles : Options -> Coordinate -> Int -> Dict String Tile
loadTiles options center zoom =
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
            |> List.map (\tileXY -> tileXY zoom)
            |> List.map (\tileXYZ -> tileXYZ Nothing)
            |> List.map (\tile -> ( tileId tile, tile ))
            |> Dict.fromList


init : Options -> ( Model, Cmd Msg )
init options =
    let
        newTiles =
            loadTiles options options.center options.zoomLevel
    in
        ( { tiles = newTiles
          , prevPosition = Point 0 0
          , dragging = False
          , center = options.center
          , zoomLevel = options.zoomLevel
          , options = options
          }
        , loadTexturesCmd options.baseUrl newTiles
        )


loadTilesCmd : Cmd Msg
loadTilesCmd =
    Task.perform (\_ -> LoadTiles) <| Task.succeed ()


isNothing : Maybe a -> Bool
isNothing maybe =
    case maybe of
        Nothing ->
            True

        Just _ ->
            False


handleLoadResult : TileId -> Result Error Texture -> Msg
handleLoadResult tileId result =
    case result of
        Err err ->
            TextureError err

        Ok texture ->
            TextureLoad tileId texture


attemptTileTasks : TileId -> Task Error Texture -> Cmd Msg
attemptTileTasks tileId task =
    Task.attempt (handleLoadResult tileId) task


loadTexturesCmd : String -> TileMap -> Cmd Msg
loadTexturesCmd baseUrl tiles =
    let
        loadOptions =
            { defaultOptions | magnify = nearest, minify = nearestMipmapNearest }

        tileLoadTasks =
            tiles
                |> Dict.toList
                |> List.map Tuple.second
                |> List.filter (\tile -> isNothing tile.texture)
                |> List.map (tileImageUrl baseUrl)
                |> List.map Texture.load

        tileIds =
            tiles
                |> Dict.toList
                |> List.map Tuple.first
    in
        List.zip tileIds tileLoadTasks
            |> List.map (uncurry attemptTileTasks)
            |> Cmd.batch


tileImageUrl : String -> Tile -> String
tileImageUrl baseUrl { x, y, z } =
    baseUrl
        ++ toString z
        ++ "/"
        ++ toString x
        ++ "/"
        ++ toString y
        ++ ".png?access_token=pk.eyJ1IjoibWFwYm94IiwiYSI6ImNpandmbXliNDBjZWd2M2x6bDk3c2ZtOTkifQ._QA7i5Mpkd_m30IGElHziw"


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
                            { lng = model.center.lng - moveLng
                            , lat = model.center.lat + moveLat
                            }
                    }
            in
                ( newModel, loadTilesCmd )

        Zoom event ->
            let
                delta =
                    if event.deltaY > 0 then
                        -1
                    else
                        1

                newModel =
                    { model | tiles = Dict.empty, zoomLevel = model.zoomLevel + round delta }
            in
                -- ( newModel, loadTilesCmd )
                ( model, Cmd.none )

        LoadTiles ->
            let
                newTiles =
                    loadTiles model.options model.center model.zoomLevel
            in
                ( { model | tiles = Dict.union model.tiles newTiles }, Cmd.none )

        TextureLoad tileId texture ->
            let
                load tile =
                    { tile | texture = Just texture }

                maybeLoad =
                    Maybe.map load

                newTiles =
                    Dict.update tileId maybeLoad model.tiles
            in
                ( { model | tiles = newTiles }, Cmd.none )

        TextureError error ->
            ( model, Cmd.none )
                |> Debug.log (toString error)
