module Main exposing (..)

import Svg exposing (..)
import Svg.Attributes exposing (..)
import Browser
import Html exposing (Html, div)

main = Browser.sandbox { init = 0, update = update, view = view }

type Msg = None

update msg model = model

view model = div []
  [svg
    [ width "1200px"
    , height "800px"
    , viewBox "0 0 100 100"
    ]
    [ Svg.path
        [ fill "white"
        , stroke "black"
        , strokeWidth "0.2"
        , d "M 0 0 l -2 2 m 2 -2 l 2 2 m -2 -2 l 0 -3 m 0 0 l 2 0 m -2 0 l -2 0 m 2 0 a 1 1 0 0 0 0 -2 a 1 1 0 0 0 0 2"
        ]
        []
    ]]
