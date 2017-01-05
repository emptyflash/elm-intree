module Intree.View exposing (..)

import Intree.Model as Model exposing (Model, MouseEvent, decodeClick, wheelEventDecoder)
import Intree.Update as Update exposing (Msg(..))
import Html exposing (..)
import Html.Attributes as Attributes exposing (..)
import Html.Events exposing (on, onWithOptions, onMouseUp)


onMouseDown : Attribute Msg
onMouseDown =
    on "mousedown" decodeClick
        |> Attributes.map StartDrag


onWheel : Attribute Msg
onWheel =
    let
        wheelOptions =
            { stopPropagation = False
            , preventDefault = True
            }
    in
        onWithOptions "wheel" wheelOptions wheelEventDecoder
            |> Attributes.map Wheel


intreeContainerStyle : Model -> Attribute msg
intreeContainerStyle model =
    style
        [ ( "height", "400px" )
        , ( "width", "600px" )
        , ( "position", "relative" )
        , grabStyle model.dragging
        ]


grabStyle : Bool -> ( String, String )
grabStyle grabbing =
    if grabbing then
        ( "cursor", "-webkit-grabbing" )
    else
        ( "cursor", "-webkit-grab" )


debugOverlay : Model -> Html msg
debugOverlay model =
    span
        []
        [ span
            [ style [ ( "position", "absolute" ), ( "top", "0" ), ( "left", "0" ) ] ]
            [ text (toString model.topLeft) ]
        , span
            [ style [ ( "position", "absolute" ), ( "top", "0" ), ( "right", "0" ) ] ]
            [ text (toString model.topRight) ]
        , span
            [ style [ ( "position", "absolute" ), ( "bottom", "0" ), ( "left", "0" ) ] ]
            [ text (toString model.bottomLeft) ]
        , span
            [ style [ ( "position", "absolute" ), ( "bottom", "0" ), ( "right", "0" ) ] ]
            [ text (toString model.bottomRight) ]
        , span
            [ style [ ( "position", "absolute" ), ( "top", "0" ), ( "left", "200px" ) ] ]
            [ text (toString model.zoomLevel) ]
        ]


intreeMapPaneStyle : Attribute msg
intreeMapPaneStyle =
    style
        [ ( "z-index", "400" )
        , ( "position", "absolute" )
        , ( "left", "0" )
        , ( "top", "0" )
        ]


view : Model -> Html Msg
view model =
    div
        -- Container
        [ onMouseDown
        , onMouseUp StopDrag
        , onWheel
        , intreeContainerStyle model
        ]
        [ div
            -- Map pane
            [ intreeMapPaneStyle ]
            []
        , div
            -- Controls
            []
            [ debugOverlay model ]
        ]
