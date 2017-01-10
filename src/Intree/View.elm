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
import WebGL exposing (Entity, Shader, Mesh)
import WebGL.Texture exposing (Texture)
import Math.Vector2 as Vec2 exposing (Vec2, vec2)
import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import Math.Matrix4 as Mat4 exposing (Mat4)


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
        [ ( "position", "absolute" )
        , ( "left", "0" )
        , ( "top", "0" )
        , ( "-webkit-user-drag", "none" )
        , ( "-webkit-user-select", "none" )
        , ( "pointer-events", "none" )
        ]


type alias Vertex =
    { position : Vec3
    , textureCoord : Vec2
    }


tileMesh : Mesh Vertex
tileMesh =
    let
        topLeft =
            { position = vec3 -1 1 0, textureCoord = vec2 0 1 }

        topRight =
            { position = vec3 1 1 0, textureCoord = vec2 1 1 }

        bottomLeft =
            { position = vec3 -1 -1 0, textureCoord = vec2 0 0 }

        bottomRight =
            { position = vec3 1 -1 0, textureCoord = vec2 1 0 }

        square =
            [ ( topLeft, topRight, bottomLeft )
            , ( bottomLeft, topRight, bottomRight )
            ]
    in
        WebGL.triangles square


type alias Varying =
    { vcoord : Vec2
    }


type alias Uniform =
    { rotation : Mat4
    , perspective : Mat4
    , camera : Mat4
    , texture : Texture
    }


tileUniform : Texture -> Uniform
tileUniform =
    Uniform
        (Mat4.makeRotate 0 (vec3 0 1 0))
        (Mat4.makePerspective 45 1 0.01 100)
        (Mat4.makeLookAt (vec3 0 3 8) (vec3 0 0 0) (vec3 0 1 0))


tileVS : Shader Vertex Uniform Varying
tileVS =
    [glsl|
        attribute vec2 textureCoord;
        attribute vec3 position;
        uniform mat4 perspective;
        uniform mat4 camera;
        uniform mat4 rotation;
        varying vec2 vcoord;

        void main () {
            gl_Position = perspective * camera * rotation * vec4(position, 1.0);
            vcoord = textureCoord;
        }
    |]


tileFS : Shader {} { u | texture : Texture } Varying
tileFS =
    [glsl|
        precision mediump float;
        uniform sampler2D texture;
        varying vec2 vcoord;

        void main () {
            gl_FragColor = texture2D(texture, vcoord);
        }
    |]


tileEntity : Model -> Tile -> Maybe Entity
tileEntity model tile =
    tile.texture
        |> Maybe.map
            (\texture ->
                WebGL.entity
                    tileVS
                    tileFS
                    tileMesh
                    (tileUniform texture)
            )


displayTiles : Model -> List Entity
displayTiles model =
    model.tiles
        |> Dict.toList
        |> List.map Tuple.second
        |> List.filterMap (tileEntity model)


view : Model -> Html Msg
view model =
    div
        []
        [ WebGL.toHtml
            [ onMouseDown
            , onWheel
            , intreeContainerStyle model
            ]
            (displayTiles model)
        , div
            -- Controls
            []
            [ debugOverlay model ]
        ]
