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
        )
import Mouse


type Msg
    = Pan Int Int
    | StartDrag Point
    | StopDrag
    | Wheel WheelEvent


subscriptions : Model -> Sub Msg
subscriptions model =
    if model.dragging then
        Sub.batch
            [ Mouse.moves (\{ x, y } -> Pan x y)
            , Mouse.ups (\_ -> StopDrag)
            ]
    else
        Sub.none


initLayer : Options -> Layer
initLayer options =
    let
        numHorizontal =
            ceiling <| toFloat options.width / toFloat options.tileSize

        numVertical =
            ceiling <| toFloat options.height / toFloat options.tileSize

        startX =
            floor options.topLeft.lng

        startY =
            floor options.topLeft.lat

        horizontalRange =
            List.range startX (startX + numHorizontal)

        verticalRange =
            List.range startY (startY + numVertical)

        almostTiles =
            List.concatMap
                (\x ->
                    List.map
                        (Tile x)
                        verticalRange
                )
                horizontalRange
    in
        horizontalRange
            |> List.map Tile
            |> List.concatMap (\tileX -> List.map tileX verticalRange)
            |> List.map (\tileXY -> tileXY options.zoomLevel)


init : Options -> ( Model, Cmd Msg )
init options =
    ( { layer = initLayer options
      , prevPosition = Point 0 0
      , dragging = False
      , topLeft = options.topLeft
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
                        , topLeft =
                            { lng = model.topLeft.lng + moveLng
                            , lat = model.topLeft.lat + moveLat
                            }
                    }
            in
                ( newModel, Cmd.none )

        Wheel event ->
            ( { model | zoomLevel = model.zoomLevel + round event.deltaY }, Cmd.none )
