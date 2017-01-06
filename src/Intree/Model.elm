module Intree.Model exposing (..)

import Json.Decode.Pipeline exposing (decode, required)
import Json.Decode exposing (float, int, Decoder)
import Set exposing (Set)


type alias Options =
    { baseUrl : String
    , width : Int
    , height : Int
    , tileSize : Int
    , center : Coordinate
    , zoomLevel : Int
    }


type alias Model =
    { tiles : Set Tile
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
    ( Int, Int, Int )


type alias Layer =
    List Tile


type alias Map =
    List Layer


wheelEventDecoder : Decoder WheelEvent
wheelEventDecoder =
    decode WheelEvent
        |> required "deltaX" float
        |> required "deltaY" float
        |> required "deltaZ" float


decodePoint : Decoder Point
decodePoint =
    decode Point
        |> required "pageX" int
        |> required "pageY" int
