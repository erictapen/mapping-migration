module Main exposing (main)

import Browser
import Graphics exposing (..)
import Html exposing (Html, br, div, fieldset, legend, option, select, text)
import Html.Attributes exposing (value)
import Html.Events exposing (onInput)
import Http exposing (get)
import Json.Decode as JD
import List exposing (map)
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
            ( { model
                | availableCountries =
                    Just <| map Country <| Result.withDefault [] countrieNames
              }
            , Cmd.none
            )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Time.every (1000 / 60) Tick


countryOption (Country cname) =
    option [ value cname ] [ text cname ]


countrySelect maybeCountries =
    case maybeCountries of
        Nothing ->
            text "Loading countries..."

        Just countries ->
            fieldset []
                [ legend [] [ text "country of origin" ]
                , select [ onInput ChangeCoo ] <| map countryOption countries
                ]


view : Model -> Browser.Document Msg
view model =
    { title = "Mapping migration"
    , body =
        [ div []
            [ countrySelect model.availableCountries
            , br [] []
            , text <| "CoO: " ++ countryName model.coo
            ]
        ]
    }
