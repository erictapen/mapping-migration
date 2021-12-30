module Main exposing (..)

import Browser
import Html exposing (Html, div, option, select, text)
import Html.Attributes exposing (value)
import Html.Events exposing (onInput)
import Http exposing (get)
import Json.Decode as JD
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
      , availableCountries = Nothing
      , stickFigures =
            [ { position = ( 0, 0 )
              , start = ( 0, 0 )
              , destination = ( 100, 100 )
              }
            ]
      }
    , Http.request
        { method = "GET"
        , headers = [ Http.header "Accept" "application/json" ]
        , url = "https://api.unhcr.org/population/v1/countries/"
        , body = Http.emptyBody
        , expect = Http.expectJson GotCountries countriesDecoder
        , timeout = Nothing
        , tracker = Nothing
        }
    )


countriesDecoder : JD.Decoder (List String)
countriesDecoder =
    JD.field "items" <| JD.list <| JD.field "name" JD.string


type Country
    = Country String


countryName (Country str) =
    str


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
    , availableCountries : Maybe (List Country)
    }


type Msg
    = Tick Time.Posix
    | ChangeCoo String
    | GotCountries (Result Http.Error (List String))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick _ ->
            ( { model | stickFigures = map moveStickFigure model.stickFigures }, Cmd.none )

        ChangeCoo str ->
            ( { model | coo = Country str }, Cmd.none )

        GotCountries countrieNames ->
            ( { model | availableCountries = Just <| map Country <| Result.withDefault [] countrieNames }, Cmd.none )


moveStickFigure : StickFigure -> StickFigure
moveStickFigure sf =
    { sf | position = ( Tuple.first sf.position + 0.1, Tuple.second sf.position + 0.1 ) }


subscriptions : Model -> Sub Msg
subscriptions _ =
    Time.every (1000 / 60) Tick


countryOption (Country cname) =
    option [ value cname ] [ text cname ]


countrySelect maybeCountries =
    case maybeCountries of
        Just countries ->
            select [ onInput ChangeCoo ] <| map countryOption countries

        Nothing ->
            text "Loading countries..."


view : Model -> Browser.Document Msg
view model =
    { title = "Mapping migration"
    , body =
        [ div []
            [ countrySelect model.availableCountries
            , text <| countryName model.coo
            ]
        ]
    }
