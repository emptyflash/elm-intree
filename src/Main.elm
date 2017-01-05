module Main exposing (..)

import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)
import Intree.Model as Intree
import Intree.Update as Intree
import Intree.View as Intree


main =
    Html.program
        { init = Intree.init
        , view = Intree.view
        , update = Intree.update
        , subscriptions = Intree.subscriptions
        }
