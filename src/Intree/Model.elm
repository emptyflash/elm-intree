module Intree.Model exposing (..)

import Json.Decode.Pipeline exposing (decode, required)
import Json.Decode exposing (float, int, Decoder)


type alias Options =
    { baseUrl : String
    , width : Int
    , height : Int
    , tileSize : Int
    , topLeft : Coordinate
    , zoomLevel : Int
    }


type alias Model =
    { layer : Layer
    , prevPosition : Point
    , dragging : Bool
    , topLeft : Coordinate
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
    }


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
