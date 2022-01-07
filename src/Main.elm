module Main exposing (main)

import Api exposing (..)
import Browser
import Dict exposing (Dict)
import Graphics exposing (..)
import Html exposing (Html, br, div, fieldset, h1, legend, option, select, text)
import Html.Attributes exposing (value)
import Html.Events exposing (onInput)
import Http exposing (get)
import List exposing (filter, head, map)
import Maybe exposing (withDefault)
import Platform.Cmd
import String exposing (fromInt)
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
    ( CountriesLoading, fetchCountries GotCountries )


{-| Some wrapper type we use to express that some stuff can:

  - Not loaded yet
  - Be in the process of loading
  - Loaded, that wraps a Maybe. Nothing signifies an error!

-}
type Loadable a
    = NotLoaded
    | Loading
    | Loaded (Maybe a)


unwrapLoadable : Loadable a -> Maybe a
unwrapLoadable loadable =
    case loadable of
        NotLoaded ->
            Nothing

        Loading ->
            Nothing

        Loaded maybe ->
            maybe


{-|

  - `CountriesLoading`: The origin state of the application, that is only used for initialization.
  - `CountriesLoadingFailed`: Error on /countries
  - `COALoading`: COA data is being requested
  - `COASelected`: COA data is loaded and displayed

-}
type Model
    = CountriesLoading
    | CountriesLoadingFailed
    | COALoading COOSelect
    | COASelected COOSelect COASelect


type COOSelect
    = COOSelect (Dict CountryCode Country) CountryCode


type COASelect
    = COASelect AvailableCOAs CountryCode


type Msg
    = Tick Time.Posix
    | GotCountries (Result Http.Error (Dict CountryCode Country))
    | ChangeCoo CountryCode
    | GotAsylumDecisions (Result Http.Error AvailableCOAs)
    | ChangeCoa CountryCode


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        noop =
            ( model, Cmd.none )
    in
    case msg of
        Tick _ ->
            ( model, Cmd.none )

        GotCountries countriesResult ->
            case countriesResult of
                Err _ ->
                    ( CountriesLoadingFailed, Cmd.none )

                Ok countries ->
                    let
                        coo =
                            withDefault unknownCountryCode <|
                                head <|
                                    List.sortWith compareCountryCode <|
                                        Dict.keys countries
                    in
                    ( COALoading (COOSelect countries coo), fetchAsylumDecisions GotAsylumDecisions coo )

        ChangeCoo countryCode ->
            case model of
                -- TODO not sure wether we should do noop in this case.
                COALoading (COOSelect countries _) ->
                    ( COALoading (COOSelect countries countryCode)
                    , fetchAsylumDecisions GotAsylumDecisions countryCode
                    )

                COASelected (COOSelect countries _) _ ->
                    ( COALoading (COOSelect countries countryCode)
                    , fetchAsylumDecisions GotAsylumDecisions countryCode
                    )

                _ ->
                    noop

        GotAsylumDecisions asylumDecisionsResult ->
            case asylumDecisionsResult of
                Err _ ->
                    ( CountriesLoadingFailed, Cmd.none )

                Ok asylumDecisions ->
                    case model of
                        COALoading (COOSelect countries coo) ->
                            let
                                coa =
                                    withDefault unknownCountryCode <|
                                        head <|
                                            List.sortWith compareCountryCode <|
                                                Dict.keys asylumDecisions
                            in
                            ( COASelected
                                (COOSelect countries coo)
                                (COASelect asylumDecisions coa)
                            , Cmd.none
                            )

                        _ ->
                            noop

        ChangeCoa countryCode ->
            case model of
                COASelected cooS (COASelect availableCOAs _) ->
                    ( COASelected cooS (COASelect availableCOAs countryCode), Cmd.none )

                _ ->
                    noop


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


countryOption : ( CountryCode, Country ) -> Html Msg
countryOption ( code, { name } ) =
    option [ value code ] [ text (name ++ " (" ++ code ++ ")") ]


cooSelect countries =
    fieldset []
        [ legend [] [ text "country of origin" ]
        , select [ onInput ChangeCoo ]
            (map countryOption <|
                List.sortWith (compareCountryCode2 Tuple.first) <|
                    Dict.toList countries
            )
        ]


coaSelect countries coas =
    if Dict.isEmpty coas then
        text "no data available."

    else
        fieldset []
            [ legend [] [ text "country of asylum" ]
            , select [ onInput ChangeCoa ] <|
                map
                    countryOption
                <|
                    map
                        (\cc ->
                            ( cc
                            , withDefault unknownCountry <|
                                Dict.get cc countries
                            )
                        )
                    <|
                        List.sortWith compareCountryCode <|
                            Dict.keys coas
            ]


coaVis : Maybe COA -> Html Msg
coaVis maybeCoa =
    case maybeCoa of
        Nothing ->
            text ""

        Just coa ->
            div [] <| map coaYearVis <| List.reverse <| List.sortBy Tuple.first <| Dict.toList coa


coaYearVis : ( Year, AsylumDecisions ) -> Html Msg
coaYearVis ( year, ad ) =
    div [] <|
        [ h1 [] [ text <| fromInt year ] ]
            ++ procedureType ad.procedureType
            ++ applicationType ad.applicationType
            ++ displayInt "decisions recognized: " ad.decisionsRecognized
            ++ displayInt "other decisions: " ad.decisionsOther
            ++ displayInt "decisions rejected: " ad.decisionsRejected
            ++ displayInt "decisions closed: " ad.decisionsClosed
            ++ [ text <| "decisions total: " ++ fromInt ad.decisionsTotal
               ]


procedureType : ProcedureType -> List (Html Msg)
procedureType pt =
    [ case pt of
        Government ->
            text "Asylum applications were processed by the local government."

        Joint ->
            text "Asylum applications were processed by both the local government and UNHCR."

        UNHCR ->
            text "Asylum applications were processed by the UNHCR."
    , br [] []
    ]


applicationType : Maybe ApplicationType -> List (Html Msg)
applicationType maybeAt =
    [ case maybeAt of
        Nothing ->
            text ""

        Just at ->
            text <|
                "application type: "
                    ++ (case at of
                            New ->
                                "New"

                            Repeat ->
                                "Repeat"

                            Appeal ->
                                "Appea"
                       )
    , br [] []
    ]


displayInt : String -> Maybe Int -> List (Html Msg)
displayInt prefix maybeInt =
    case maybeInt of
        Nothing ->
            []

        Just dr ->
            [ text <| prefix ++ fromInt dr, br [] [] ]


view : Model -> Browser.Document Msg
view model =
    { title = "Mapping migration"
    , body =
        case model of
            CountriesLoading ->
                [ text "Loading countries..." ]

            CountriesLoadingFailed ->
                [ text "An error occured while fetching the countries!" ]

            COALoading (COOSelect countries _) ->
                [ cooSelect countries, text "loading..." ]

            COASelected (COOSelect countries selectedCOO) (COASelect availableCOAs selectedCOA) ->
                [ cooSelect countries
                , coaSelect countries availableCOAs
                , coaVis <| Dict.get selectedCOA availableCOAs
                , br [] []
                , Html.pre []
                    [ text <|
                        "curl -H 'Accept: application/json' '"
                            ++ asylumDecisionsPath selectedCOO
                            ++ "'"
                    ]
                ]
    }
