module Intree.Model exposing (..)

import Json.Decode.Pipeline exposing (decode, required)
import Json.Decode exposing (float, int, Decoder)


type alias Context =
    { baseUrl : String
    }


type alias Model =
    { mousePosition : MouseEvent
    , prevPosition : MouseEvent
    , dragging : Bool
    , topLeft : Coordinate
    , topRight : Coordinate
    , bottomLeft : Coordinate
    , bottomRight : Coordinate
    , zoomLevel : Int
    }


type alias Coordinate =
    { lat : Int
    , lng : Int
    }


type alias WheelEvent =
    { deltaX : Float
    , deltaY : Float
    , deltaZ : Float
    }


type alias MouseEvent =
    { x : Int
    , y : Int
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


decodeClick : Decoder MouseEvent
decodeClick =
    decode MouseEvent
        |> required "pageX" int
        |> required "pageY" int
