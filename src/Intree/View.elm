module Intree.View exposing (..)

import Intree.Model as Model
    exposing
        ( Model
        , Coordinate
        , Tile
        , Options
        , tileId
        , decodePoint
        , decodeWheelEvent
        , decodeLoadEvent
        )
import Intree.Update as Update exposing (Msg(..))
import Html exposing (Attribute, Html, text, div, img, span)
import Html.Attributes as Attributes exposing (..)
import Html.Events exposing (on, onWithOptions, onMouseUp)
import Html.Keyed exposing (node)
import Dict


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
        onWithOptions "wheel" wheelOptions decodeWheelEvent
            |> Attributes.map Zoom


onLoad : Attribute Msg
onLoad =
    on "load" decodeLoadEvent
        |> Attributes.map ImageLoaded


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
            [ text (toString model.center) ]
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
tileImgStyle model { x, y, loaded } =
    let
        calcOffset pos coord dimenson =
            toString <| truncate <| (toFloat pos - coord) * toFloat model.options.tileSize - (toFloat dimenson / 2.0)

        translate =
            "translate3d("
                ++ calcOffset x model.center.lng model.options.width
                ++ "px, "
                ++ calcOffset y model.center.lat model.options.height
                ++ "px, 0px)"
    in
        style
            [ ( "transform", translate )
            , ( "transition", "opacity 0.2s linear" )
            , ( "opacity"
              , if loaded then
                    "1"
                else
                    "0"
              )
            , ( "-webkit-user-drag", "none" )
            , ( "-webkit-user-select", "none" )
            , ( "width", "256px" )
            , ( "height", "256px" )
            , ( "position", "absolute" )
            , ( "top", "0" )
            , ( "left", "0" )
            ]


tileImgSrc : Model -> Tile -> Attribute msg
tileImgSrc model { x, y, z } =
    model.options.baseUrl
        ++ toString z
        ++ "/"
        ++ toString x
        ++ "/"
        ++ toString y
        ++ ".png?access_token=pk.eyJ1IjoibWFwYm94IiwiYSI6ImNpandmbXliNDBjZWd2M2x6bDk3c2ZtOTkifQ._QA7i5Mpkd_m30IGElHziw"
        |> src


tileImg : Model -> Tile -> Html Msg
tileImg model tile =
    img
        [ tileImgSrc model tile
        , draggable "false"
        , tileImgStyle model tile
        , id <| tileId tile
        , onLoad
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
        [ model.tiles
            |> Dict.toList
            |> List.map (\( id, tile ) -> ( id, tileImg model tile ))
            |> node "div" [ intreeMapPaneStyle ]
        , div
            -- Controls
            []
            [ debugOverlay model ]
        ]
