module Main exposing (main)

import Api exposing (..)
import Browser
import Data
import Dict exposing (Dict)
import Html
    exposing
        ( Html
        , br
        , div
        , fieldset
        , h1
        , h2
        , input
        , legend
        , option
        , select
        , text
        )
import Html.Attributes as HA exposing (class, id, type_, value)
import Html.Events exposing (onInput)
import Http exposing (get)
import List exposing (filter, head, map)
import Maybe exposing (withDefault)
import Platform.Cmd
import Set exposing (Set)
import String exposing (fromFloat, fromInt)
import Svg as S exposing (Svg, circle, g, rect, svg, text_)
import Svg.Attributes as SA
    exposing
        ( cx
        , cy
        , fill
        , height
        , preserveAspectRatio
        , r
        , stroke
        , style
        , textAnchor
        , viewBox
        , width
        , x
        , y
        )
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
    = COASelect AvailableCOAs CountryCode (Maybe CountryCode) Year


type Msg
    = Tick Time.Posix
    | GotCountries (Result Http.Error (Dict CountryCode Country))
    | ChangeCoo CountryCode
    | GotAsylumDecisions (Result Http.Error AvailableCOAs)
    | ChangeCoa1 CountryCode
    | ChangeCoa2 CountryCode
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
                                coa n =
                                    head <|
                                        List.drop n <|
                                            map Tuple.first <|
                                                filteredAndSortedCOAs countries availableCOAs

                                coa1 =
                                    withDefault unknownCountryCode <| coa 0

                                coa2 =
                                    coa 1

                                year =
                                    withDefault "unknownYear" <|
                                        Maybe.andThen List.maximum <|
                                            Maybe.map Set.toList <|
                                                Maybe.map
                                                    (Set.fromList
                                                        >> Set.union
                                                            (Set.fromList <|
                                                                withDefault [] <|
                                                                    Maybe.map Dict.keys <|
                                                                        Dict.get (withDefault "" coa2) availableCOAs
                                                            )
                                                    )
                                                <|
                                                    Maybe.map Dict.keys <|
                                                        Dict.get coa1 availableCOAs
                            in
                            ( COASelected
                                (COOSelect countries coo)
                                (COASelect availableCOAs coa1 coa2 year)
                            , Cmd.none
                            )

                        _ ->
                            noop

        ChangeCoa1 selectedCoa1 ->
            case model of
                COASelected cooS (COASelect availableCOAs _ selectedCoa2 year) ->
                    ( COASelected cooS (COASelect availableCOAs selectedCoa1 selectedCoa2 year), Cmd.none )

                _ ->
                    noop

        ChangeCoa2 selectedCoa2 ->
            case model of
                COASelected cooS (COASelect availableCOAs selectedCoa1 _ year) ->
                    ( COASelected cooS
                        (COASelect
                            availableCOAs
                            selectedCoa1
                            (if String.isEmpty selectedCoa2 then
                                Nothing

                             else
                                Just selectedCoa2
                            )
                            year
                        )
                    , Cmd.none
                    )

                _ ->
                    noop

        ChangeYear year ->
            case model of
                COASelected cooS (COASelect availableCOAs coa1 coa2 _) ->
                    ( COASelected cooS (COASelect availableCOAs coa1 coa2 year), Cmd.none )

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
        div []
            [ fieldset []
                [ legend [] [ text "country of asylum" ]
                , select [ onInput ChangeCoa1 ] <|
                    map countryOption <|
                        filteredAndSortedCOAs countries coas
                ]
            , fieldset []
                [ legend [] [ text "country of asylum" ]
                , select [ onInput ChangeCoa2 ] <|
                    map countryOption <|
                        filteredAndSortedCOAs countries coas
                ]
            ]


yearOption : String -> Html Msg
yearOption year =
    option [ value year ] [ text year ]


yearInput : List Year -> Html Msg
yearInput years =
    if List.length years <= 1 then
        text ""

    else
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
    div []
        ([ h2 [] [ text countryName ] ]
            ++ (case maybeAsylumDecisions of
                    Nothing ->
                        [ text "No data for this year." ]

                    Just ad ->
                        case maybePopulation of
                            Err e ->
                                [ text e ]

                            Ok population ->
                                [ text <|
                                    let
                                        count =
                                            ad.total * perCapitaUnit // population
                                    in
                                    "Per "
                                        ++ perCapitaUnitString
                                        ++ (case count of
                                                1 ->
                                                    " inhabitants there was "
                                                        ++ fromInt count
                                                        ++ " decision in total"

                                                _ ->
                                                    " inhabitants there were "
                                                        ++ fromInt count
                                                        ++ " decisions in total"
                                           )
                                , coaSvg ad
                                , br [] []
                                ]
                                    ++ displayPersonsOrCases ad.personsOrCases
                                    ++ displayInt "otherwise closed: " ad.closed population
                                    ++ displayInt "complementary protection: " ad.other population
                                    ++ displayInt "recognized: " ad.recognized population
                                    ++ displayInt "rejected: " ad.rejected population
               )
        )


coaSvg : AsylumDecisions -> Html Msg
coaSvg ad =
    let
        barElements =
            [ barElement
                ad.total
                (withDefault 0 ad.recognized)
                0
                "recognized"
                "#d8d8d8"
            , barElement
                ad.total
                (withDefault 0 ad.other)
                (withDefault 0 ad.recognized)
                "complimentary protection"
                "#e6e6e6"
            , barElement
                ad.total
                (withDefault 0 ad.closed)
                (withDefault 0 ad.recognized + withDefault 0 ad.other)
                "otherwise closed"
                "#eeeeee"
            , barElement
                ad.total
                (withDefault 0 ad.rejected)
                (withDefault 0 ad.recognized + withDefault 0 ad.other + withDefault 0 ad.closed)
                "rejected"
                "#f5f5f5"
            ]
    in
    div []
        [ svg
            [ width "100"
            , height "100"
            , style "width:100%"
            ]
            [ svg
                [ viewBox "0 0 100 100"
                , id "bar"
                , preserveAspectRatio "none"
                ]
              <|
                map Tuple.first barElements
            ]
        , div [ style "position: relative; width: 100%; margin-bottom: 3em;" ] <| map Tuple.second barElements
        ]


barElement : Int -> Int -> Int -> String -> String -> ( Svg Msg, Html Msg )
barElement total dividend position textContent color =
    let
        xPos =
            fromFloat <| 100 * (toFloat position / toFloat total)

        width =
            fromFloat <| 100 * (toFloat dividend / toFloat total)
    in
    ( g []
        [ rect
            [ x xPos
            , SA.width width
            , height "100"
            , stroke "none"
            , fill color
            ]
            []
        , svg
            [ viewBox "0 0 100 100"
            , x (xPos ++ "%")
            , SA.width (width ++ "%")
            , preserveAspectRatio "xMidYMax meet"
            ]
            []
        ]
    , div
        [ style <|
            "overflow: hidden; "
                ++ "text-overflow: ellipsis; "
                ++ "position: absolute; "
                ++ "top: 80%; "
                ++ "text-align: center; "
                ++ "left: "
                ++ xPos
                ++ "%; "
                ++ "width: "
                ++ width
                ++ "%; "
        ]
        [ S.text <| (fromInt <| round <| 100 * (toFloat dividend / toFloat total)) ++ "% " ++ textContent ]
    )


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
                [ h1 [] [ text "Seeking Asylum" ]
                , text "Loading countries..."
                ]

            CountriesLoadingFailed ->
                [ text "An error occured while fetching the countries!" ]

            COALoading (COOSelect countries _) ->
                [ div [ id "menu", class "base" ]
                    [ h1 [] [ text "Seeking Asylum" ]
                    , cooSelect countries
                    , text "loading..."
                    ]
                ]

            COASelected (COOSelect countries selectedCOO) (COASelect availableCOAs selectedCOA1 selectedCOA2 selectedYear) ->
                [ div [ id "menu", class "base" ]
                    [ h1 [] [ text "Seeking Asylum" ]
                    , cooSelect countries
                    , br [] []
                    , coaSelect countries availableCOAs
                    , div []
                        [ let
                            years countryCode =
                                Set.fromList <|
                                    withDefault [] <|
                                        Maybe.map Dict.keys <|
                                            Maybe.andThen (\cc -> Dict.get cc availableCOAs) countryCode
                          in
                          yearInput <| Set.toList <| Set.union (years <| Just selectedCOA1) (years selectedCOA2)
                        ]
                    , text selectedYear
                    ]
                , div [ id "vis", class "base" ]
                    [ coaVis
                        (withDefault "Country Name not found!" <|
                            Maybe.map .name <|
                                Dict.get selectedCOA1 countries
                        )
                        (coaPopulation countries selectedCOA1)
                      <|
                        Maybe.andThen (Dict.get selectedYear) <|
                            Dict.get selectedCOA1 availableCOAs
                    , case selectedCOA2 of
                        Nothing ->
                            text ""

                        Just sCOA2 ->
                            coaVis
                                (withDefault "Country Name not found!" <|
                                    Maybe.map .name <|
                                        Dict.get sCOA2 countries
                                )
                                (coaPopulation countries sCOA2)
                            <|
                                Maybe.andThen (Dict.get selectedYear) <|
                                    Dict.get sCOA2 availableCOAs
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
