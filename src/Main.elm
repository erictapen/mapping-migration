module Main exposing (main)

import Api exposing (..)
import Browser
import Data
import Dict exposing (Dict)
import Html exposing (Html, br, div, fieldset, h1, input, legend, option, select, text)
import Html.Attributes as HA exposing (class, id, type_, value)
import Html.Events exposing (onInput)
import Http exposing (get)
import List exposing (filter, head, map)
import Maybe exposing (withDefault)
import Platform.Cmd
import String exposing (fromFloat, fromInt)
import Svg as S exposing (Svg, rect, svg)
import Svg.Attributes as SA exposing (fill, height, preserveAspectRatio, stroke, viewBox, width, x, y)
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
    = COASelect AvailableCOAs CountryCode Year


type Msg
    = Tick Time.Posix
    | GotCountries (Result Http.Error (Dict CountryCode Country))
    | ChangeCoo CountryCode
    | GotAsylumDecisions (Result Http.Error AvailableCOAs)
    | ChangeCoa CountryCode
    | ChangeYear Year


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

        ChangeCoo selectedCoo ->
            case model of
                COASelected (COOSelect countries _) _ ->
                    ( COALoading (COOSelect countries selectedCoo)
                    , fetchAsylumDecisions GotAsylumDecisions selectedCoo
                    )

                _ ->
                    noop

        GotAsylumDecisions asylumDecisionsResult ->
            case asylumDecisionsResult of
                Err _ ->
                    ( CountriesLoadingFailed, Cmd.none )

                Ok availableCOAs ->
                    case model of
                        COALoading (COOSelect countries coo) ->
                            let
                                coa =
                                    withDefault unknownCountryCode <|
                                        head <|
                                            map Tuple.first <|
                                                filteredAndSortedCOAs countries availableCOAs

                                year =
                                    withDefault "unknownYear" <|
                                        Maybe.andThen List.maximum <|
                                            Maybe.map Dict.keys <|
                                                Dict.get coa availableCOAs
                            in
                            ( COASelected
                                (COOSelect countries coo)
                                (COASelect availableCOAs coa year)
                            , Cmd.none
                            )

                        _ ->
                            noop

        ChangeCoa selectedCoa ->
            case model of
                COASelected cooS (COASelect availableCOAs _ _) ->
                    let
                        year =
                            withDefault "unknownYear" <|
                                Maybe.andThen List.maximum <|
                                    Maybe.map Dict.keys <|
                                        Dict.get selectedCoa availableCOAs
                    in
                    ( COASelected cooS (COASelect availableCOAs selectedCoa year), Cmd.none )

                _ ->
                    noop

        ChangeYear year ->
            case model of
                COASelected cooS (COASelect availableCOAs coa _) ->
                    ( COASelected cooS (COASelect availableCOAs coa year), Cmd.none )

                _ ->
                    noop


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


countryOption : ( CountryCode, Country ) -> Html Msg
countryOption ( code, { name } ) =
    option [ value code ] [ text (name ++ " (" ++ code ++ ")") ]


{-| We only want european countries in sorted order.
-}
filteredAndSortedCOAs : Dict CountryCode Country -> AvailableCOAs -> List ( CountryCode, Country )
filteredAndSortedCOAs countries coas =
    List.filterMap
        (\cc ->
            let
                maybeCountry =
                    Dict.get cc countries
            in
            case maybeCountry of
                Nothing ->
                    Nothing

                Just country ->
                    if List.member country.iso Data.europeanCountries then
                        Just ( cc, country )

                    else
                        Nothing
        )
    <|
        List.sortWith compareCountryCode <|
            Dict.keys coas


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
                map countryOption <|
                    filteredAndSortedCOAs countries coas
            ]


yearOption : String -> Html Msg
yearOption year =
    option [ value year ] [ text year ]


yearInput : List Year -> Html Msg
yearInput years =
    input
        [ type_ "range"
        , onInput ChangeYear
        , HA.min <| withDefault "0" <| List.minimum years
        , HA.max <| withDefault "0" <| List.maximum years
        ]
    <|
        map yearOption years


coaVis : String -> Result String Int -> Maybe AsylumDecisions -> Html Msg
coaVis countryName maybePopulation maybeAsylumDecisions =
    case maybeAsylumDecisions of
        Nothing ->
            text "No data for this year."

        Just ad ->
            case maybePopulation of
                Err e ->
                    text e

                Ok population ->
                    div []
                        ([ h1 [] [ text countryName ]
                         , text <|
                            "Per " ++ perCapitaUnitString ++ " inhabitants there were "
                                ++ fromInt (ad.total * perCapitaUnit // population)
                                ++ " decisions in total"
                         , coaSvg ad
                         , br [] []
                         ]
                            ++ displayPersonsOrCases ad.personsOrCases
                            ++ displayInt
                                ("decisions recognized per " ++ perCapitaUnitString ++ " inhabitants: ")
                                ad.recognized
                                population
                            ++ displayInt
                                ("other decisions per " ++ perCapitaUnitString ++ " inhabitants: ")
                                ad.other
                                population
                            ++ displayInt
                                ("decisions rejected per " ++ perCapitaUnitString ++ " inhabitants: ")
                                ad.rejected
                                population
                            ++ displayInt
                                ("decisions closed per " ++ perCapitaUnitString ++ " inhabitants: ")
                                ad.closed
                                population
                        )


coaSvg : AsylumDecisions -> Html Msg
coaSvg ad =
    svg
        [ width "1"
        , height "1"
        , viewBox "0 0 1 1"
        , id "bar"
        , preserveAspectRatio "none"
        ]
        [ barElement
            ad.total
            (withDefault 0 ad.closed)
            0
            "other"
            "#c6c6c6"
        , barElement
            ad.total
            (withDefault 0 ad.other)
            (withDefault 0 ad.closed)
            "complimentary_protection"
            "#afafaf"
        , barElement
            ad.total
            (withDefault 0 ad.recognized)
            (withDefault 0 ad.closed + withDefault 0 ad.other)
            "recognized"
            "#8e8d8d"
        , barElement
            ad.total
            (withDefault 0 ad.rejected)
            (withDefault 0 ad.closed + withDefault 0 ad.other + withDefault 0 ad.recognized)
            "rejected"
            "#6d6d6d"
        ]


barElement : Int -> Int -> Int -> String -> String -> Svg Msg
barElement total dividend position id color =
    rect
        [ x <| fromFloat <| toFloat position / toFloat total
        , width <| fromFloat <| toFloat dividend / toFloat total
        , height "1"
        , stroke "none"
        , fill color
        , SA.id id
        ]
        []


{-| Granularity in which we calculate asylum decision count in relation to population of the COA
-}
perCapitaUnit =
    500000


perCapitaUnitString =
    "500,000"


displayInt : String -> Maybe Int -> Int -> List (Html Msg)
displayInt prefix maybeInt div =
    case maybeInt of
        Nothing ->
            []

        Just dr ->
            [ text <| prefix ++ fromInt ((dr * perCapitaUnit) // div), br [] [] ]


displayPersonsOrCases : PersonsOrCases -> List (Html Msg)
displayPersonsOrCases pOrC =
    [ case pOrC of
        Persons ->
            text "This data is based on Person counts."

        Cases ->
            text "This data is based on Cases counts."

        Mixed ->
            text "This data is based on both Cases and Person counts!"
    , br [] []
    ]


coaPopulation : Dict CountryCode Country -> CountryCode -> Result String Int
coaPopulation countries cc =
    case Dict.get cc countries of
        Nothing ->
            Err <| "Country " ++ cc ++ "not available."

        Just country ->
            Result.fromMaybe ("No population data for " ++ country.iso ++ ".") <|
                (Dict.get country.iso >> Maybe.andThen (Dict.get 2018)) Data.population


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
                [ div [ id "menu", class "base" ]
                    [ cooSelect countries, text "loading..." ]
                ]

            COASelected (COOSelect countries selectedCOO) (COASelect availableCOAs selectedCOA selectedYear) ->
                [ div [ id "menu", class "base" ]
                    [ cooSelect countries
                    , coaSelect countries availableCOAs
                    , div []
                        [ yearInput <|
                            withDefault [] <|
                                Maybe.map Dict.keys <|
                                    Dict.get selectedCOA availableCOAs
                        ]
                    , text selectedYear
                    ]
                , div [ id "vis", class "base" ]
                    [ coaVis
                        (withDefault "Country Name not found!" <|
                            Maybe.map .name <|
                                Dict.get selectedCOA countries
                        )
                        (coaPopulation countries selectedCOA)
                      <|
                        Maybe.andThen (Dict.get selectedYear) <|
                            Dict.get selectedCOA availableCOAs
                    , br [] []
                    , Html.pre []
                        [ text <|
                            "curl -H 'Accept: application/json' '"
                                ++ asylumDecisionsPath selectedCOO
                                ++ "'"
                        ]
                    ]
                ]
    }
