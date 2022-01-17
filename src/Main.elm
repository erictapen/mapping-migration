module Main exposing (main)

import Api exposing (..)
import Browser
import Data
import Dict exposing (Dict)
import Html
    exposing
        ( Html
        , a
        , br
        , div
        , fieldset
        , h1
        , h2
        , h3
        , hr
        , input
        , legend
        , li
        , ol
        , option
        , p
        , select
        , sup
        , text
        )
import Html.Attributes as HA exposing (attribute, class, href, id, title, type_, value)
import Html.Events exposing (onInput)
import Http exposing (get)
import List exposing (filter, head, map)
import Maybe exposing (withDefault)
import Platform.Cmd
import Set exposing (Set)
import String exposing (fromFloat, fromInt)
import Svg as S exposing (Svg, circle, g, path, rect, svg, symbol, text_, use)
import Svg.Attributes as SA
    exposing
        ( cx
        , cy
        , d
        , fill
        , height
        , preserveAspectRatio
        , r
        , stroke
        , style
        , textAnchor
        , transform
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
    | COAsNotAvailable


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
                                coa1 =
                                    withDefault unknownCountryCode <|
                                        head <|
                                            map Tuple.first <|
                                                filteredAndSortedCOAs countries availableCOAs

                                coa2 =
                                    Just coa1

                                year =
                                    Maybe.andThen List.minimum <|
                                        Maybe.map Set.toList <|
                                            Maybe.map
                                                (Set.fromList
                                                    >> Set.union
                                                        (Set.fromList <|
                                                            withDefault [] <|
                                                                Maybe.map Dict.keys <|
                                                                    Dict.get
                                                                        (withDefault "" coa2)
                                                                        availableCOAs
                                                        )
                                                )
                                            <|
                                                Maybe.map Dict.keys <|
                                                    Dict.get coa1 availableCOAs
                            in
                            case year of
                                Just y ->
                                    ( COASelected
                                        (COOSelect countries coo)
                                        (COASelect availableCOAs coa1 coa2 y)
                                    , Cmd.none
                                    )

                                Nothing ->
                                    ( COASelected
                                        (COOSelect countries coo)
                                        COAsNotAvailable
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
    option [ value code ] [ text name ]


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


coaVis : CountryCode -> Maybe Country -> Result String Int -> Maybe AsylumDecisions -> Html Msg
coaVis countryCode country maybePopulation maybeAsylumDecisions =
    div []
        ([ h2
            [ title <|
                String.append ("UNHCR: " ++ countryCode) <|
                    withDefault "" <|
                        Maybe.map (.iso >> String.append ", ISO: ") country
            ]
            [ text <| withDefault "Unkown country name!" <| Maybe.map .name country ]
         ]
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
                                                0 ->
                                                    " inhabitants there were less than 1 decisions in total"

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
                "#a8a8a8"
            , barElement
                ad.total
                (withDefault 0 ad.other)
                (withDefault 0 ad.recognized)
                "complementary protection"
                "#b7b7b7"
            , barElement
                ad.total
                (withDefault 0 ad.closed)
                (withDefault 0 ad.recognized + withDefault 0 ad.other)
                "otherwise closed"
                "#cecece"
            , barElement
                ad.total
                (withDefault 0 ad.rejected)
                (withDefault 0 ad.recognized + withDefault 0 ad.other + withDefault 0 ad.closed)
                "rejected"
                "#dddddd"
            ]

        rectangle ( r, _, _ ) =
            r

        footprints ( _, f, _ ) =
            f

        legend ( _, _, l ) =
            l
    in
    div []
        [ svg
            [ width "100"
            , height "15em"
            , style "width:100%"
            ]
            ([ footprint1
             , svg
                [ viewBox "0 0 100 100"
                , id "bar"
                , preserveAspectRatio "none"
                ]
               <|
                map rectangle barElements
             ]
                ++ map footprints barElements
            )
        , div [ style "position: relative; width: 100%; margin-bottom: 3em;" ] <| map legend barElements
        ]


barElement : Int -> Int -> Int -> String -> String -> ( Svg Msg, Svg Msg, Html Msg )
barElement total dividend position textContent color =
    let
        xPos =
            fromFloat <| 100 * (toFloat position / toFloat total)

        width =
            100 * (toFloat dividend / toFloat total)
    in
    ( rect
        [ x xPos
        , SA.width <| fromFloat width
        , height "100"
        , stroke "none"
        , fill color
        ]
        []
    , svg
        [ viewBox <| "0 0 "++ (fromFloat <| width * 4)  ++ " 100"
        , x (xPos ++ "%")
        , SA.width (fromFloat width ++ "%")
        , preserveAspectRatio "xMinYMin meet"
        ]
        [ use
            [ attribute "href" "#fs1"
            , y "50%"
            ]
            []
        , use
            [ attribute "href" "#fs1"
            , y "50%"
            , x "80"
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
                ++ fromFloat width
                ++ "%; "
        ]
        [ S.text <| (fromInt <| round <| 100 * (toFloat dividend / toFloat total)) ++ "% " ++ textContent
        , a [ href "#" ] [ text "ⓘ" ]
        ]
    )


footprint1 : Svg Msg
footprint1 =
    symbol [ id "fs1" ]
        [ path [ stroke "none", fill "black", d "M 2.12064 2.21568 C 2.48736 2.20482 2.7805 2.19638 3.00005 2.19035 C 3.46328 2.17708 3.29801 -0.00515408 2.59593 0.0696378 C 2.00966 0.13116 1.65742 2.23016 2.12064 2.21568 Z" ] []
        , path [ stroke "none", fill "black", d "M 2.10737 2.47625 C 2.25937 2.47142 2.94335 2.44971 3.01935 2.44729 C 3.17135 2.44247 3.09052 3.19521 3.01211 3.38461 C 2.9325 3.58003 2.22559 3.51489 2.14959 3.42321 C 2.03138 3.27966 1.95538 2.48107 2.10737 2.47625 Z" ] []
        , path [ stroke "none", fill "black", d "M 0.309957 3.4027 C 0.676678 3.38461 0.969814 3.37134 1.19057 3.36048 C 1.655 3.33756 1.32809 1.1698 0.629631 1.25786 C 0.0445662 1.33024 -0.154477 3.42441 0.309957 3.4027 Z" ] []
        , path [ stroke "none", fill "black", d "M 0.303925 3.67895 C 0.455921 3.67171 1.14111 3.63914 1.21711 3.63552 C 1.36911 3.62828 1.33895 4.36534 1.27501 4.55232 C 1.20867 4.74533 0.494524 4.69226 0.412494 4.60419 C 0.282211 4.46426 0.151929 3.68619 0.303925 3.67895 Z" ] []
        ]


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


introduction : Html Msg
introduction =
    div [ style "clear: both; padding-top: 5em;", id "introduction" ]
        [ h2 []
            [ text """
Introduction // "How States Grant Asylum" // "Making Refugees"
"""
            ]
        , p [] [ text """
Most people would probably associate the term "refugee" with a person that has had to escape from his or her home to search and hopefully find a new place to live that offers safety, fair living conditions and new opportunities. The word evokes so many images of human bodies on the move, in camps, or in detention centers, it almost seems as if being a “refugee” were the natural state of a particular, homogenous group of people. As if this word were not a political one. But it is. Refugees are not simply “refugees”, they are labelled as such (cf. Zetter 2007). Refugees are not born “refugees”, they simply have had to react to the events disrupting the place they have coincidentally been born into. A reaction that is made out to be unnatural by a system that wants to establish lines and borders, legal classifications and categories as the “normal (…) order of things” (Malkki 1992, 26), rather than movement and fluidity.
""" ]
        , p [] [ text """
The label “refugee” is a political and judicial construction. In fact, looking from an administrative perspective, not anybody escaping their former home is considered a “refugee” officially. Before being recognized as a "refugee", displaced persons are considered "asylum seekers".
""" ]
        , h3 [] [ text "Asylum seeker" ]
        , p [] [ text """
An asylum seeker is a person applying for or claiming international  protection as a refugee who has not received a decision about their claim yet. Legally and statistically, they are not refugees yet.
""" ]
        , h3 [] [ text "Refugee" ]
        , p [] [ text """
A person who falls under one or several of a set of international agreements on refugee status, most importantly the 1951 Geneva Convention and its 1967 Protocol.
These define a refugee as "someone who is unable or unwilling to return to their country of origin owing to a well-founded fear of being persecuted for the reasons of race, religion, nationality, membership of a particular social group, or political opinion." (UNHCR 2021)
""" ]
        , br [] []
        , p []
            [ text """
In order to be recognized as a refugee and granted protection under international law, asylum seekers have to apply for asylum in a specific country. That nation-state then has the power to decide if the circumstances of a person's displacement fit the legal criteria to be granted asylum and thereby recognized as a "refugee".
"""
            , a [ href "#fn1", attribute "role" "doc-noteref", id "fnref1" ] [ sup [] [ text "1" ] ]
            , text """
Nation-states as well as international organizations judge which bodies move “legitimately” and which bodies move without meeting the expectations of what a “real” refugee would have had to endure and come from. They get to decide “[w]ho the ‘real’ refugees [are]” (Zetter 2007, 176) and whose (forced) mobility will be disclaimed as "irregular".
"""
            ]
        , p [] [ text """
The international legal basis for decisions on asylum is the 1951 Convention relating to the Status of Refugees (also known as the Geneva Convention) and its succeeding Protocol from 1967. Since then, there has been no equal successor to these documents, despite the changes the world has seen. As a result, the Geneva Convention does not do justice to current migratory movements and questions of displacement, nor to the rapid globalization that has been taking place since these documents were agreed upon. Hence, countries of asylum have established supplementary categories to grant asylum-seekers (temporary) protection in cases that are not covered by the Geneva Convention. A lot of asylum-seekers are assigned to sub-categories that offer less certainty and are subject to sudden changes in their asylum decision (see explanation of "complementary protection"). They are classified and categorized according to a complex legal system that fractions one label into many (cf. Zetter 2007, 181), increasingly reinforcing the distinction between "genuine refugees" and asylum-seekers. One of the regions where this bureaucratic classification is most rigorously pursued and serves a restrictive migration governance is Europe.
""" ]
        , p [] [ text """
Through our web application, we want to make decisions on asylum in Europe, their proportions and developments since the year 2000 more easily accessible and visible using UNHCR data. Looking at this data, the fragmentation of asylum decisions beyond "recognized" and "rejected" is inherently visible due to the further categories of "complementary protection" and "otherwise closed". However, these categories represent a summarization of a variety of asylum-decisions and cannot do justice to the complexity of asylum decisions in each individual country.
""" ]
        , p [] [ text """
Our webapp enables the user to select a country of origin and two European countries of asylum. It then visualizes the decisions made on asylum applications in these countries of asylum in the chosen year. To contextualize these numbers and offer the opportunity for comparison between and within countries, two simple visual components are used:  A bar chart shows the share of each decision category in relation to the total number of decisions in a given country. Footprints set the numbers in relation to the number of inhabitants: 1 footprint represents 1 person per 500,000 inhabitants. With this combination, the user can explore and compare the proportional distribution of decisions made within and between countries as well as get a rough idea of absolute numbers of decisions made. A slider provides the possibility to explore each year from 2000-2021 individually, but also to observe temporal evolutions and changes.
""" ]
        , hr [] []
        , ol []
            [ p []
                [ li [ id "fn1", attribute "role" "doc-endnote" ]
                    [ p []
                        [ text """
In some cases, large groups of people fleeing at the same time from similar circumstances are granted asylum and thereby recognized as refugees without having formally applied for asylum. They are called “prima facie refugees”. Also, it is not always nation-states who manage questions of asylum; some get support by the UNHCR. This, however, is currently barely ever the case in Europe, where each country has full control over who gets to stay and who has to leave.
"""
                        , a [ href "#fnref1", attribute "role" "doc-backlink" ] [ text "↩︎" ]
                        ]
                    ]
                ]
            ]
        ]


menu : List (Html Msg) -> Html Msg
menu html =
    div
        [ style <|
            "float: left;"
                ++ " width: 24em;"
                ++ " margin-right: 5em;"
        ]
        ([ h1 [] [ text "Seeking Asylum" ] ]
            ++ html
            ++ [ p []
                    [ text "Scroll down for an "
                    , a [ href "#introduction" ] [ text "introduction" ]
                    , text "."
                    ]
               ]
        )


view : Model -> Browser.Document Msg
view model =
    { title = "Mapping migration"
    , body =
        case model of
            CountriesLoading ->
                [ div []
                    [ menu
                        [ text "Loading countries..."
                        ]
                    , introduction
                    ]
                ]

            CountriesLoadingFailed ->
                [ text "An error occured while fetching the countries!" ]

            COALoading (COOSelect countries _) ->
                [ div []
                    [ menu
                        [ cooSelect countries
                        , text "loading..."
                        ]
                    , introduction
                    ]
                ]

            COASelected (COOSelect countries selectedCOO) coaS ->
                case coaS of
                    COAsNotAvailable ->
                        [ div []
                            [ menu
                                [ cooSelect countries
                                , text <|
                                    String.append "Unfortunately there is no data available for " <|
                                        .name <|
                                            withDefault unknownCountry <|
                                                Dict.get selectedCOO countries
                                ]
                            , introduction
                            ]
                        ]

                    COASelect availableCOAs selectedCOA1 selectedCOA2 selectedYear ->
                        [ div []
                            [ menu
                                [ cooSelect countries
                                , br [] []
                                , coaSelect countries availableCOAs
                                , div []
                                    [ let
                                        years countryCode =
                                            Set.fromList <|
                                                withDefault [] <|
                                                    Maybe.map Dict.keys <|
                                                        Maybe.andThen
                                                            (\cc -> Dict.get cc availableCOAs)
                                                            countryCode
                                      in
                                      yearInput <|
                                        Set.toList <|
                                            Set.union (years <| Just selectedCOA1) (years selectedCOA2)
                                    ]
                                , text selectedYear
                                ]
                            , div [ style <| "float: left;" ++ " width: 60%;" ]
                                [ coaVis
                                    selectedCOA1
                                    (Dict.get selectedCOA1 countries)
                                    (coaPopulation countries selectedCOA1)
                                  <|
                                    Maybe.andThen (Dict.get selectedYear) <|
                                        Dict.get selectedCOA1 availableCOAs
                                , case selectedCOA2 of
                                    Nothing ->
                                        text ""

                                    Just sCOA2 ->
                                        coaVis
                                            sCOA2
                                            (Dict.get sCOA2 countries)
                                            (coaPopulation countries sCOA2)
                                        <|
                                            Maybe.andThen (Dict.get selectedYear) <|
                                                Dict.get sCOA2 availableCOAs
                                ]
                            ]
                        , introduction
                        ]
    }
