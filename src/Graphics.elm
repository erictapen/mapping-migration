module Graphics exposing (..)

import Svg exposing (g, svg)
import Svg.Attributes exposing (..)


stickFigure ( x, y ) =
    g
        [ transform ("translate(" ++ String.fromFloat x ++ "," ++ String.fromFloat y ++ ")")
        ]
        [ Svg.path
            [ fill "white"
            , stroke "black"
            , strokeWidth "0.2"
            , d "M 0 0 l -2 2 m 2 -2 l 2 2 m -2 -2 l 0 -3 m 0 0 l 2 0 m -2 0 l -2 0 m 2 0 a 1 1 0 0 0 0 -2 a 1 1 0 0 0 0 2"
            ]
            []
        ]


type alias Point =
    ( Float, Float )


type alias StickFigure =
    { position : Point
    , start : Point
    , destination : Point
    }


moveStickFigure : StickFigure -> StickFigure
moveStickFigure sf =
    { sf | position = ( Tuple.first sf.position + 0.1, Tuple.second sf.position + 0.1 ) }
