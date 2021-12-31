module Main exposing (main)

import Api exposing (Country, fetchCountries, handleGotCountries, unknownCountry)
import Browser
import Graphics exposing (..)
import Html exposing (Html, br, div, fieldset, legend, option, select, text)
import Html.Attributes exposing (value)
import Html.Events exposing (onInput)
import Http exposing (get)
import List exposing (filter, head, map)
import Maybe exposing (withDefault)
import Platform.Cmd
import Task
import Time


main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { coo = unknownCountry
      , coa = unknownCountry
      , availableCountries = Nothing
      , stickFigures =
            [ { position = ( 0, 0 )
              , start = ( 0, 0 )
              , destination = ( 100, 100 )
              }
            ]
      }
    , fetchCountries GotCountries
    )


type alias Model =
    { coo : Country
    , coa : Country
    , stickFigures : List StickFigure
    , availableCountries : Maybe (List Country)
    }


type Msg
    = Tick Time.Posix
    | ChangeCoo String
    | GotCountries (Result Http.Error (List Country))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick _ ->
            ( { model | stickFigures = map moveStickFigure model.stickFigures }, Cmd.none )

        ChangeCoo countryCode ->
            let
                coo =
                    withDefault unknownCountry <|
                        head <|
                            filter (\x -> x.code == countryCode) <|
                                withDefault [] model.availableCountries
            in
            ( { model | coo = coo }, Cmd.none )

        GotCountries countryNames ->
            handleGotCountries model countryNames


subscriptions : Model -> Sub Msg
subscriptions _ =
    Time.every (1000 / 60) Tick


cooOption { name, code } =
    option [ value code ] [ text name ]


cooSelect maybeCountries =
    case maybeCountries of
        Nothing ->
            text "Loading countries..."

        Just countries ->
            fieldset []
                [ legend [] [ text "country of origin" ]
                , select [ onInput ChangeCoo ] <| map cooOption countries
                ]


view : Model -> Browser.Document Msg
view model =
    { title = "Mapping migration"
    , body =
        [ div []
            [ cooSelect model.availableCountries
            , br [] []
            , text <| "CoO: " ++ .name model.coo ++ " (" ++ .code model.coo ++ ")"
            ]
        ]
    }
