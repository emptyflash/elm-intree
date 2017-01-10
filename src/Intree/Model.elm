module Intree.Model exposing (..)

import Json.Decode.Pipeline exposing (decode, required)
import Json.Decode as Decode exposing (float, int, string, Decoder)
import Dict exposing (Dict)
import WebGL.Texture as Texture exposing (Texture)


type alias Options =
    { baseUrl : String
    , width : Int
    , height : Int
    , tileSize : Int
    , center : Coordinate
    , zoomLevel : Int
    }


type alias Model =
    { tiles : TileMap
    , prevPosition : Point
    , dragging : Bool
    , center : Coordinate
    , zoomLevel : Int
    , options : Options
    }


type alias Coordinate =
    { lat : Float
    , lng : Float
    }


type alias Point =
    { x : Int
    , y : Int
    }


type alias WheelEvent =
    { deltaX : Float
    , deltaY : Float
    , deltaZ : Float
    }


type alias Tile =
    { x : Int
    , y : Int
    , z : Int
    , texture : Maybe Texture
    }


tileId : Tile -> TileId
tileId { x, y, z } =
    toString x ++ toString y ++ toString z


type alias TileMap =
    Dict TileId Tile


type alias TileId =
    String


type alias Layer =
    List Tile


type alias Map =
    List Layer


decodeWheelEvent : Decoder WheelEvent
decodeWheelEvent =
    decode WheelEvent
        |> required "deltaX" float
        |> required "deltaY" float
        |> required "deltaZ" float


decodePoint : Decoder Point
decodePoint =
    decode Point
        |> required "pageX" int
        |> required "pageY" int


decodeLoadEvent : Decoder String
decodeLoadEvent =
    Decode.at [ "target", "id" ] string
