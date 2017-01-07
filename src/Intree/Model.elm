module Intree.Model exposing (..)

import Json.Decode.Pipeline exposing (decode, required)
import Json.Decode as Decode exposing (float, int, string, Decoder)
import Dict exposing (Dict)


type alias Options =
    { baseUrl : String
    , width : Int
    , height : Int
    , tileSize : Int
    , center : Coordinate
    , zoomLevel : Int
    }


type alias Model =
    { tiles : Dict String Tile
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
    , loaded : Bool
    }


tileId : Tile -> String
tileId { x, y, z } =
    toString x ++ toString y ++ toString z


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
