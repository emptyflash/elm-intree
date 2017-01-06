module Intree.View exposing (..)

import Intree.Model as Model
    exposing
        ( Model
        , Coordinate
        , Tile
        , Options
        , decodePoint
        , wheelEventDecoder
        )
import Intree.Update as Update exposing (Msg(..))
import Html exposing (..)
import Html.Attributes as Attributes exposing (..)
import Html.Events exposing (on, onWithOptions, onMouseUp)


onMouseDown : Attribute Msg
onMouseDown =
    on "mousedown" decodePoint
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
        [ ( "height", toString model.options.height ++ "px" )
        , ( "width", toString model.options.width ++ "px" )
        , ( "position", "relative" )
        , ( "overflow", "hidden" )
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
            [ text (toString model.zoomLevel) ]
        ]


intreeMapPaneStyle : Attribute msg
intreeMapPaneStyle =
    style
        [ ( "z-index", "400" )
        , ( "position", "absolute" )
        , ( "left", "0" )
        , ( "top", "0" )
        , ( "-webkit-user-drag", "none" )
        , ( "-webkit-user-select", "none" )
        , ( "pointer-events", "none" )
        ]


tileImgStyle : Model -> Tile -> Attribute msg
tileImgStyle model tile =
    let
        calcOffset pos coord =
            toString <| truncate <| (toFloat pos - coord) * toFloat model.options.tileSize

        translate =
            "translate3d("
                ++ calcOffset tile.x model.topLeft.lng
                ++ "px, "
                ++ calcOffset tile.y model.topLeft.lat
                ++ "px, 0px)"
    in
        style
            [ ( "transform", translate )
            , ( "-webkit-user-drag", "none" )
            , ( "-webkit-user-select", "none" )
            , ( "width", "256px" )
            , ( "height", "256px" )
            , ( "position", "absolute" )
            , ( "top", "0" )
            , ( "left", "0" )
            ]


tileImgSrc : Model -> Tile -> Attribute msg
tileImgSrc model tile =
    model.options.baseUrl
        ++ toString tile.z
        ++ "/"
        ++ toString tile.x
        ++ "/"
        ++ toString tile.y
        ++ ".png?access_token=pk.eyJ1IjoibWFwYm94IiwiYSI6ImNpandmbXliNDBjZWd2M2x6bDk3c2ZtOTkifQ._QA7i5Mpkd_m30IGElHziw"
        |> src


tileImg : Model -> Tile -> Html Msg
tileImg model tile =
    img
        [ tileImgSrc model tile
        , draggable "false"
        , tileImgStyle model tile
        ]
        []


view : Model -> Html Msg
view model =
    div
        -- Container
        [ onMouseDown
        , onWheel
        , intreeContainerStyle model
        ]
        [ model.layer
            |> List.map (tileImg model)
            |> div [ intreeMapPaneStyle ]
        , div
            -- Controls
            []
            [ debugOverlay model ]
        ]
