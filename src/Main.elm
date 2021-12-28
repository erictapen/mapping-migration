module Main exposing (..)

import Browser
import Html exposing (Html, div)
import List exposing (map)
import Platform.Cmd
import Svg exposing (g, svg)
import Svg.Attributes exposing (..)
import Task
import Time


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


main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { coo = Country "Afghanistan"
      , coa = Country "Pakistan"
      , stickFigures =
            [ { position = ( 0, 0 )
              , start = ( 0, 0 )
              , destination = ( 100, 100 )
              }
            ]
      }
    , Cmd.none
    )


type Country
    = Country String


type alias Point =
    ( Float, Float )


type alias StickFigure =
    { position : Point
    , start : Point
    , destination : Point
    }


type alias Model =
    { coo : Country
    , coa : Country
    , stickFigures : List StickFigure
    }


type Msg
    = Tick Time.Posix


update : Msg -> Model -> ( Model, Cmd Msg )
update (Tick _) model =
    ( { model | stickFigures = map moveStickFigure model.stickFigures }, Cmd.none )


moveStickFigure : StickFigure -> StickFigure
moveStickFigure sf =
    { sf | position = ( Tuple.first sf.position + 0.1, Tuple.second sf.position + 0.1 ) }


subscriptions : Model -> Sub Msg
subscriptions _ =
    Time.every (1000 / 60) Tick


view : Model -> Browser.Document Msg
view model =
    { title = "Mapping migration"
    , body =
        [ div []
            [ svg
                [ width "1200px"
                , height "800px"
                , viewBox "0 0 100 100"
                ]
                (map (.position >> stickFigure) model.stickFigures)
            ]
        ]
    }
