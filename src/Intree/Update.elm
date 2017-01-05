module Intree.Update exposing (..)

import Intree.Model as Model exposing (Model, WheelEvent, MouseEvent)
import Mouse


type Msg
    = Pan Int Int
    | StartDrag MouseEvent
    | StopDrag
    | Wheel WheelEvent


subscriptions : Model -> Sub Msg
subscriptions model =
    if model.dragging then
        Mouse.moves (\{ x, y } -> Pan x y)
    else
        Sub.none


init : ( Model, Cmd Msg )
init =
    ( { mousePosition = MouseEvent 0 0
      , prevPosition = MouseEvent 0 0
      , dragging = False
      , topLeft = { lat = 0, lng = 0 }
      , topRight = { lat = 10, lng = 0 }
      , bottomLeft = { lat = 0, lng = 10 }
      , bottomRight = { lat = 10, lng = 10 }
      , zoomLevel = 0
      }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        StartDrag pos ->
            ( { model | dragging = True, mousePosition = pos }, Cmd.none )

        StopDrag ->
            ( { model | dragging = False }, Cmd.none )

        Pan x y ->
            let
                moveLng =
                    model.prevPosition.x - model.mousePosition.x

                moveLat =
                    model.mousePosition.y - model.prevPosition.y

                newModel =
                    { model
                        | mousePosition = MouseEvent x y
                        , prevPosition = model.mousePosition
                        , topLeft =
                            { lng = model.topLeft.lng - moveLng
                            , lat = model.topLeft.lat + moveLat
                            }
                        , topRight =
                            { lng = model.topRight.lng - moveLng
                            , lat = model.topRight.lat + moveLat
                            }
                        , bottomLeft =
                            { lng = model.bottomLeft.lng - moveLng
                            , lat = model.bottomLeft.lat + moveLat
                            }
                        , bottomRight =
                            { lng = model.bottomRight.lng - moveLng
                            , lat = model.bottomRight.lat + moveLat
                            }
                    }
            in
                ( newModel, Cmd.none )

        Wheel event ->
            ( { model | zoomLevel = model.zoomLevel + round event.deltaY }, Cmd.none )
