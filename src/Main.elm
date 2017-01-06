module Main exposing (..)

import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)
import Intree.Model as Intree
import Intree.Update as Intree
import Intree.View as Intree


main =
    let
        options =
            { baseUrl = "https://api.tiles.mapbox.com/v4/mapbox.streets/"
            , width = 600
            , height = 400
            , tileSize = 256
            , center = { lng = 16375.5, lat = 10895.5 }
            , zoomLevel = 15
            }
    in
        Html.program
            { init = Intree.init options
            , view = Intree.view
            , update = Intree.update
            , subscriptions = Intree.subscriptions
            }
