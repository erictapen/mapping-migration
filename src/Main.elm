module Main exposing (main)

import Api exposing (..)
import Browser
import Browser.Events
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
        , sup
        , text
        )
import Html.Attributes as HA exposing (attribute, class, href, id, title, type_, value)
import Html.Events exposing (onInput)
import Html.Styled exposing (toUnstyled)
import Http exposing (get)
import Introduction
import List exposing (filter, head, map)
import Maybe exposing (withDefault)
import Platform.Cmd
import Random exposing (Seed, initialSeed, int, step)
import Select
import Set exposing (Set)
import Simplex
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
    | COASelected COOSelect COASelect AnimationComponent


type alias AnimationComponent =
    Float


type COOSelect
    = COOSelect (Dict CountryCode Country) CountryCode Select.State


type COASelect
    = COASelect AvailableCOAs CountryCode Select.State CountryCode Select.State Year
    | COAsNotAvailable


type Msg
    = Tick Time.Posix
    | GotCountries (Result Http.Error (Dict CountryCode Country))
    | ChangeCoo (Select.Msg CountryCode)
    | GotAsylumDecisions (Result Http.Error AvailableCOAs)
    | ChangeCoa1 (Select.Msg CountryCode)
    | ChangeCoa2 (Select.Msg CountryCode)
    | ChangeYear Year


initialAnimationState =
    400


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        noop =
            ( model, Cmd.none )
    in
    case msg of
        Tick _ ->
            case model of
                COASelected cooS coaS animationState ->
                    ( COASelected cooS
                        coaS
                        (if animationState > 0 then
                            animationState - 10

                         else
                            0
                        )
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )

        GotCountries countriesResult ->
            case countriesResult of
                Err _ ->
                    ( CountriesLoadingFailed, Cmd.none )

                Ok countries ->
                    let
                        -- We temporarily filter Aruba, as it is a country without data coming right at the top.
                        filteredCountries =
                            Dict.filter (\k -> \_ -> k /= "ABW") countries

                        coo =
                            withDefault unknownCountryCode <|
                                head <|
                                    List.sortWith compareCountryCode <|
                                        Dict.keys filteredCountries
                    in
                    ( COALoading (COOSelect filteredCountries coo Select.initState), fetchAsylumDecisions GotAsylumDecisions coo )

        ChangeCoo selectedCooMsg ->
            case model of
                COASelected (COOSelect countries selectedCoo cooSelectState) coaS animationState ->
                    let
                        ( maybeAction, updatedSelectState, cmds ) =
                            Select.update selectedCooMsg cooSelectState

                        updatedCoo =
                            case maybeAction of
                                Just (Select.Select c) ->
                                    c

                                _ ->
                                    selectedCoo
                    in
                    if updatedCoo /= selectedCoo then
                        ( COALoading (COOSelect countries updatedCoo updatedSelectState)
                        , Cmd.batch
                            [ fetchAsylumDecisions GotAsylumDecisions selectedCoo
                            , Cmd.map ChangeCoo cmds
                            ]
                        )

                    else
                        ( COASelected (COOSelect countries selectedCoo updatedSelectState) coaS animationState
                        , Cmd.map ChangeCoo cmds
                        )

                _ ->
                    noop

        GotAsylumDecisions asylumDecisionsResult ->
            case asylumDecisionsResult of
                Err _ ->
                    ( CountriesLoadingFailed, Cmd.none )

                Ok availableCOAs ->
                    case model of
                        COALoading (COOSelect countries coo cooSelectState) ->
                            let
                                coa1 =
                                    withDefault unknownCountryCode <|
                                        head <|
                                            map Tuple.first <|
                                                filteredAndSortedCOAs countries availableCOAs

                                coa2 =
                                    coa1

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
                                                                        coa2
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
                                        (COOSelect countries coo cooSelectState)
                                        (COASelect availableCOAs coa1 Select.initState coa2 Select.initState y)
                                        initialAnimationState
                                    , Cmd.none
                                    )

                                Nothing ->
                                    ( COASelected
                                        (COOSelect countries coo cooSelectState)
                                        COAsNotAvailable
                                        initialAnimationState
                                    , Cmd.none
                                    )

                        _ ->
                            noop

        ChangeCoa1 selectedCoa1Msg ->
            case model of
                COASelected cooS (COASelect availableCOAs selectedCoa1 coaSelectState1 selectedCoa2 coaSelectState2 year) animationState ->
                    let
                        ( maybeAction, updatedSelectState, cmds ) =
                            Select.update selectedCoa1Msg coaSelectState1

                        updatedCoa1 =
                            case maybeAction of
                                Just (Select.Select c) ->
                                    c

                                _ ->
                                    selectedCoa1
                    in
                    ( COASelected cooS
                        (COASelect
                            availableCOAs
                            updatedCoa1
                            updatedSelectState
                            selectedCoa2
                            coaSelectState2
                            year
                        )
                        animationState
                    , Cmd.map ChangeCoa1 cmds
                    )

                _ ->
                    noop

        ChangeCoa2 selectedCoa2Msg ->
            case model of
                COASelected cooS (COASelect availableCOAs selectedCoa1 coaSelectState1 selectedCoa2 coaSelectState2 year) animationState ->
                    let
                        ( maybeAction, updatedSelectState, cmds ) =
                            Select.update selectedCoa2Msg coaSelectState2

                        updatedCoa2 =
                            case maybeAction of
                                Just (Select.Select c) ->
                                    c

                                _ ->
                                    selectedCoa2
                    in
                    ( COASelected cooS
                        (COASelect
                            availableCOAs
                            selectedCoa1
                            coaSelectState1
                            updatedCoa2
                            updatedSelectState
                            year
                        )
                        animationState
                    , Cmd.map ChangeCoa2 cmds
                    )

                _ ->
                    noop

        ChangeYear year ->
            case model of
                COASelected cooS (COASelect availableCOAs selectedCoa1 coaSelectState1 selectedCoa2 coaSelectState2 _) _ ->
                    ( COASelected cooS
                        (COASelect
                            availableCOAs
                            selectedCoa1
                            coaSelectState1
                            selectedCoa2
                            coaSelectState2
                            year
                        )
                        initialAnimationState
                    , Cmd.none
                    )

                _ ->
                    noop


subscriptions : Model -> Sub Msg
subscriptions _ =
    Time.every 60 Tick


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


countrySelectElement :
    String
    -> (Select.Msg CountryCode -> Msg)
    -> List ( CountryCode, Country )
    -> Maybe (Select.MenuItem CountryCode)
    -> Select.State
    -> Html Msg
countrySelectElement identifier msgConstructor countries selectedItem selectState =
    toUnstyled <|
        Html.Styled.map msgConstructor <|
            Select.view
                (Select.single selectedItem
                    |> Select.state selectState
                    |> (Select.menuItems <| map menuItem countries)
                    |> Select.placeholder identifier
                )
                (Select.selectIdentifier identifier)


menuItem : ( CountryCode, Country ) -> Select.MenuItem CountryCode
menuItem ( cc, country ) =
    { item = cc, label = country.name }


cooSelect : Dict CountryCode Country -> CountryCode -> Select.State -> Html Msg
cooSelect countries selectedCountry =
    countrySelectElement "selectCOO"
        ChangeCoo
        (Dict.toList countries)
        (Maybe.map (\c -> menuItem ( selectedCountry, c )) <| Dict.get selectedCountry countries)


coaSelect :
    Dict CountryCode Country
    -> AvailableCOAs
    -> CountryCode
    -> Select.State
    -> CountryCode
    -> Select.State
    -> Html Msg
coaSelect countries coas selectedCOA1 selectedCOA1State selectedCOA2 selectedCOA2State =
    if Dict.isEmpty coas then
        text "no data available."

    else
        let
            selectedItem cc =
                Maybe.map (\c -> menuItem ( cc, c )) <| Dict.get cc countries
        in
        div []
            [ countrySelectElement
                "selectCOA1"
                ChangeCoa1
                (filteredAndSortedCOAs countries coas)
                (selectedItem selectedCOA1)
                selectedCOA1State
            , countrySelectElement
                "selectCOA2"
                ChangeCoa2
                (filteredAndSortedCOAs countries coas)
                (selectedItem selectedCOA2)
                selectedCOA2State
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


footprint1 : Svg Msg
footprint1 =
    symbol [ id "fs1" ]
        [ path [ stroke "none", fill "black", d "M 2.12064 2.21568 C 2.48736 2.20482 2.7805 2.19638 3.00005 2.19035 C 3.46328 2.17708 3.29801 -0.00515408 2.59593 0.0696378 C 2.00966 0.13116 1.65742 2.23016 2.12064 2.21568 Z" ] []
        , path [ stroke "none", fill "black", d "M 2.10737 2.47625 C 2.25937 2.47142 2.94335 2.44971 3.01935 2.44729 C 3.17135 2.44247 3.09052 3.19521 3.01211 3.38461 C 2.9325 3.58003 2.22559 3.51489 2.14959 3.42321 C 2.03138 3.27966 1.95538 2.48107 2.10737 2.47625 Z" ] []
        , path [ stroke "none", fill "black", d "M 0.309957 3.4027 C 0.676678 3.38461 0.969814 3.37134 1.19057 3.36048 C 1.655 3.33756 1.32809 1.1698 0.629631 1.25786 C 0.0445662 1.33024 -0.154477 3.42441 0.309957 3.4027 Z" ] []
        , path [ stroke "none", fill "black", d "M 0.303925 3.67895 C 0.455921 3.67171 1.14111 3.63914 1.21711 3.63552 C 1.36911 3.62828 1.33895 4.36534 1.27501 4.55232 C 1.20867 4.74533 0.494524 4.69226 0.412494 4.60419 C 0.282211 4.46426 0.151929 3.68619 0.303925 3.67895 Z" ] []
        ]


footprint2 : Svg Msg
footprint2 =
    symbol [ id "fs2" ]
        [ path [ stroke "none", fill "black", d "M 2.57663 2.75491 C 2.90113 2.68856 3.16049 2.63669 3.35591 2.59688 C 3.76606 2.51364 3.11948 0.172175 2.50908 0.356742 C 1.9988 0.511151 2.16528 2.83814 2.57663 2.75491 Z" ] []
        , path [ stroke "none", fill "black", d "M 2.62006 3.00582 C 2.75517 2.97808 3.36074 2.85503 3.42829 2.84176 C 3.5634 2.81402 3.62854 3.48232 3.59477 3.65965 C 3.55978 3.8418 2.91681 3.89247 2.83237 3.82491 C 2.69968 3.71634 2.48495 3.03236 2.62006 3.00582 Z" ] []
        , path [ stroke "none", fill "black", d "M 0.389574 3.1168 C 0.714074 3.18315 0.973433 3.23623 1.16765 3.27604 C 1.5778 3.35927 1.88903 0.993679 1.25451 0.924919 C 0.727343 0.867015 -0.0205751 3.03357 0.389574 3.1168 Z" ] []
        , path [ stroke "none", fill "black", d "M 0.309957 3.4027 C 0.445064 3.43045 1.05064 3.55349 1.11819 3.56797 C 1.2533 3.59571 1.05184 4.23627 0.951719 4.38585 C 0.847975 4.54026 0.236371 4.33519 0.185706 4.23989 C 0.104882 4.08789 0.174849 3.37495 0.309957 3.4027 Z" ] []
        ]


footprint3 : Svg Msg
footprint3 =
    symbol [ id "fs3" ]
        [ path [ stroke "none", fill "black", d "M 2.36046 3.01764 C 2.68134 3.10208 2.93708 3.16843 3.12889 3.2191 C 3.53421 3.32525 3.94315 1.09959 3.31586 0.992231 C 2.79232 0.902964 1.95514 2.91149 2.36046 3.01764 Z" ] []
        , path [ stroke "none", fill "black", d "M 2.2917 3.16723 C 2.42439 3.20221 3.02273 3.35903 3.08908 3.37713 C 3.22177 3.41211 2.98654 4.0406 2.87676 4.18415 C 2.76458 4.33253 2.16504 4.09368 2.1204 3.99597 C 2.04923 3.84035 2.159 3.13224 2.2917 3.16723 Z" ] []
        , path [ stroke "none", fill "black", d "M 0.739165 3.15396 C 1.06367 3.08761 1.32302 3.03453 1.51724 2.99472 C 1.92739 2.91028 1.30975 0.732872 0.69815 0.915027 C 0.19029 1.06702 0.329016 3.2384 0.739165 3.15396 Z" ] []
        , path [ stroke "none", fill "black", d "M 0.777768 3.30233 C 0.912875 3.27459 1.51845 3.15034 1.586 3.13707 C 1.72111 3.10932 1.78746 3.77762 1.75368 3.95495 C 1.7187 4.13831 1.07573 4.18898 0.992492 4.12143 C 0.858591 4.01286 0.643866 3.32887 0.777768 3.30233 Z" ] []
        ]


footprint4 : Svg Msg
footprint4 =
    symbol [ id "fs4" ]
        [ path [ stroke "none", fill "black", d "M 2.2917 3.16723 C 2.61861 3.22151 2.87918 3.26494 3.07581 3.29751 C 3.48957 3.36627 3.69224 1.11166 3.05771 1.0622 C 2.52814 1.02118 1.87914 3.09847 2.2917 3.16723 Z" ] []
        , path [ stroke "none", fill "black", d "M 2.21691 3.57617 C 2.35202 3.59909 2.96241 3.70042 3.02997 3.71128 C 3.16507 3.7342 2.98775 4.38078 2.89245 4.53399 C 2.79473 4.69201 2.17589 4.50986 2.12161 4.41577 C 2.03717 4.2686 2.0818 3.55325 2.21691 3.57617 Z" ] []
        , path [ stroke "none", fill "black", d "M 0.620946 2.44223 C 0.94424 2.36864 1.20239 2.31074 1.3954 2.26611 C 1.80435 2.17322 1.13966 0.0090805 0.532885 0.204504 C 0.0274365 0.366151 0.212003 2.53391 0.620946 2.44223 Z" ] []
        , path [ stroke "none", fill "black", d "M 0.690912 2.85238 C 0.824814 2.82222 1.42797 2.6847 1.49553 2.67022 C 1.62943 2.64006 1.71025 3.30595 1.68009 3.48449 C 1.64873 3.66785 1.00817 3.73299 0.922526 3.66664 C 0.787418 3.56169 0.557011 2.88253 0.690912 2.85238 Z" ] []
        ]


{-| SVG containing footprints for one decisison category
-}
footprintDiagram : AnimationComponent -> Seed -> Simplex.PermutationTable -> Bool -> Int -> ( Float, Float ) -> List (Svg Msg)
footprintDiagram animationState seed permTable elevatedRow count ( xPos, yPerc ) =
    let
        -- base distance
        dy =
            6.5

        dx =
            sqrt 3 * 0.5 * dy

        -- noise function we use to move the footsteps around
        noise =
            Simplex.fractal2d { scale = 0.5, steps = 7, stepSize = 2.0, persistence = 2.0 } permTable

        noiseStrength =
            3.0
    in
    case count of
        0 ->
            []

        _ ->
            let
                ( symbolIndex, nextSeed ) =
                    step (int 1 4) seed

                yPos =
                    yPerc
                        - (if elevatedRow then
                            0.5 * dy

                           else
                            0
                          )
            in
            use
                [ attribute "href"
                    (String.append "#fs" <| fromInt symbolIndex)
                , y <| fromFloat <| yPos + noiseStrength * noise yPos (xPos + 1000)
                , x <| fromFloat <| animationState + xPos + noiseStrength * noise xPos yPos
                ]
                []
                :: (if yPerc > 5 then
                        footprintDiagram
                            animationState
                            nextSeed
                            permTable
                            elevatedRow
                            (count - 1)
                            ( xPos, yPerc - dy )

                    else
                        footprintDiagram
                            animationState
                            nextSeed
                            permTable
                            (not elevatedRow)
                            (count - 1)
                            ( xPos + dx, 95 )
                   )


{-| Produces different elements for each decision category:

  - SVG consisting of the rectangle
  - SVG consisting of footprints
  - HTML that describes the category

-}
barElement : AnimationComponent -> Int -> Int -> String -> String -> String -> Int -> Int -> ( Svg Msg, Svg Msg, Html Msg )
barElement animationState dividend position textContent explanation color total population =
    let
        xPos =
            100 * (toFloat position / toFloat total)

        width =
            100 * (toFloat dividend / toFloat total)

        footprintCount =
            dividend * perCapitaUnit // population
    in
    ( rect
        [ x <| fromFloat xPos
        , SA.width <| fromFloat width
        , height "100"
        , stroke "none"
        , fill color
        ]
        []
    , svg
        [ viewBox <| "0 -100 " ++ (fromFloat <| width * 4) ++ " 300"
        , x (fromFloat (xPos / 2) ++ "%")
        , SA.width (fromFloat width ++ "%")
        , preserveAspectRatio "xMinYMin meet"
        ]
        ([ S.title []
            [ text <| fromInt footprintCount ++ " decisions/" ++ perCapitaUnitString ++ " inhabitants"
            ]
         ]
            ++ footprintDiagram
                animationState
                (initialSeed <| population + position)
                (Simplex.permutationTableFromInt <| population + position)
                False
                footprintCount
                ( 5, 95 )
        )
    , div
        [ style <|
            "overflow: hidden; "
                ++ "text-overflow: ellipsis; "
                ++ "position: absolute; "
                ++ "top: 80%; "
                ++ "text-align: center; "
                ++ "left: "
                ++ fromFloat xPos
                ++ "%; "
                ++ "width: "
                ++ fromFloat width
                ++ "%; "
        ]
        [ S.text <| (fromInt <| round <| 100 * (toFloat dividend / toFloat total)) ++ "% " ++ textContent
        , a [ href "#", title explanation ] [ text "ⓘ" ]
        ]
    )


{-| The SVG component of a COA chart
-}
coaSvg : AnimationComponent -> Int -> AsylumDecisions -> Html Msg
coaSvg animationState population ad =
    let
        barElements =
            map (\f -> f ad.total population)
                [ barElement
                    animationState
                    (withDefault 0 ad.recognized)
                    0
                    "recognized"
                    "explanation for recognized decision category"
                    "#a8a8a8"
                , barElement
                    animationState
                    (withDefault 0 ad.other)
                    (withDefault 0 ad.recognized)
                    "complementary protection"
                    "explanation for complementary protection decision category"
                    "#b7b7b7"
                , barElement
                    animationState
                    (withDefault 0 ad.closed)
                    (withDefault 0 ad.recognized + withDefault 0 ad.other)
                    "otherwise closed"
                    "explanation for otherwise closed decision category"
                    "#cecece"
                , barElement
                    animationState
                    (withDefault 0 ad.rejected)
                    (withDefault 0 ad.recognized + withDefault 0 ad.other + withDefault 0 ad.closed)
                    "rejected"
                    "explanation for rejected decision category"
                    "#dddddd"
                ]

        rectangle ( r, _, _ ) =
            r

        footprints ( _, f, _ ) =
            f

        legend ( _, _, l ) =
            l
    in
    div [ style "height: 15em;" ]
        [ svg
            [ width "200"
            , height "300%"
            , style "width: 200%; top: -15em; position: relative;"
            ]
            ([ footprint1
             , footprint2
             , footprint3
             , footprint4
             , svg
                [ viewBox "0 -100 200 300"
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


{-| A complete chart for one COA.
-}
coaVis : AnimationComponent -> Year -> CountryCode -> Maybe Country -> Result String Int -> Maybe AsylumDecisions -> Html Msg
coaVis animationState year countryCode country maybePopulation maybeAsylumDecisions =
    div [ style "margin-bottom: 4em;" ]
        ([ h2
            [ title <|
                String.append ("UNHCR: " ++ countryCode) <|
                    withDefault "" <|
                        Maybe.map (.iso >> String.append ", ISO: ") country
            ]
            [ text <| withDefault "Unkown country name!" <| Maybe.map .name country ]
         ]
            ++ (case maybePopulation of
                    Err e ->
                        [ text e ]

                    Ok population ->
                        case maybeAsylumDecisions of
                            Nothing ->
                                [ text "No UNHCR data available."
                                , div [ style "height: 15em;" ] []
                                ]

                            Just ad ->
                                [ text <|
                                    let
                                        count =
                                            ad.total * perCapitaUnit // population
                                    in
                                    "Per "
                                        ++ perCapitaUnitString
                                        ++ (case count of
                                                0 ->
                                                    " inhabitants there were less than 1 decision in total"

                                                1 ->
                                                    " inhabitants there was 1 decision in total"

                                                _ ->
                                                    " inhabitants there were "
                                                        ++ fromInt count
                                                        ++ " decisions in total"
                                           )
                                , coaSvg animationState population ad
                                ]
               )
        )


{-| The legend that explains what one footprint symbolises
-}
footprintLegend : Html Msg
footprintLegend =
    div
        [ style <|
            "padding-left: 1em;"
                ++ " margin-top: 2em;"
                ++ " margin-bottom: 2em"
        ]
        [ svg
            [ width "1em"
            , height "1em"
            , viewBox "0 0 5 5"
            ]
            [ footprint1
            , use
                [ attribute "href" "#fs1"
                , y "0"
                , x "0"
                ]
                []
            ]
        , text <| "1 decision per " ++ perCapitaUnitString ++ " inhabitants"
        , a [ href "#", title "explanation for footprints" ]
            [ text "ⓘ"
            , div [ style "width: 100px; background-color: black; position: absolute; visibility: hidden;" ] [ text "coming soon" ]
            ]
        ]


{-| Granularity in which we calculate asylum decision count in relation to population of the COA
-}
perCapitaUnit =
    500000


perCapitaUnitString =
    "500,000"


coaPopulation : Dict CountryCode Country -> CountryCode -> Result String Int
coaPopulation countries cc =
    case Dict.get cc countries of
        Nothing ->
            Err <| "Country " ++ cc ++ "not available."

        Just country ->
            Result.fromMaybe "No population data for available." <|
                (Dict.get country.iso >> Maybe.andThen (Dict.get 2018)) Data.population


menu : List (Html Msg) -> Html Msg
menu html =
    div
        [ style <|
            "float: left;"
                ++ " width: 24em;"
                ++ " margin-left: 3em;"
                ++ " margin-right: 2em;"
        ]
        ([ h1 [] [ text appTitle ] ]
            ++ html
            ++ [ p []
                    [ text "Scroll down for an "
                    , a [ href "#introduction" ] [ text "introduction" ]
                    , text "."
                    ]
               ]
        )


appTitle =
    "How European States Decide on Asylum"


view : Model -> Browser.Document Msg
view model =
    { title = appTitle
    , body =
        case model of
            CountriesLoading ->
                [ div []
                    [ menu
                        [ text "Loading countries..."
                        ]
                    , Introduction.introduction
                    ]
                ]

            CountriesLoadingFailed ->
                [ text "An error occured while fetching the countries!" ]

            COALoading (COOSelect countries selectedCOO cooSelectState) ->
                [ div []
                    [ menu
                        [ cooSelect countries selectedCOO cooSelectState
                        , text "loading..."
                        ]
                    , Introduction.introduction
                    ]
                ]

            COASelected (COOSelect countries selectedCOO cooSelectState) coaS animationState ->
                case coaS of
                    COAsNotAvailable ->
                        [ div []
                            [ menu
                                [ cooSelect countries selectedCOO cooSelectState
                                , text <|
                                    (String.append "Unfortunately there is no data available for " <|
                                        .name <|
                                            withDefault unknownCountry <|
                                                Dict.get selectedCOO countries
                                    )
                                        ++ " yet."
                                ]
                            , Introduction.introduction
                            ]
                        ]

                    COASelect availableCOAs selectedCOA1 coaSelectState1 selectedCOA2 coaSelectState2 selectedYear ->
                        [ div []
                            [ menu
                                [ cooSelect countries selectedCOO cooSelectState
                                , br [] []
                                , coaSelect countries availableCOAs selectedCOA1 coaSelectState1 selectedCOA2 coaSelectState2
                                , footprintLegend
                                , div []
                                    [ let
                                        years countryCode =
                                            Set.fromList <|
                                                withDefault [] <|
                                                    Maybe.map Dict.keys <|
                                                        Dict.get countryCode availableCOAs
                                      in
                                      yearInput <|
                                        Set.toList <|
                                            Set.union (years selectedCOA1) (years selectedCOA2)
                                    ]
                                , p [ style "font-size: 4em; margin-top: 0;" ] [ text selectedYear ]
                                ]
                            , div
                                [ style <|
                                    "float: left;"
                                        ++ " width: 60%;"
                                        ++ " margin-left: 3em;"
                                ]
                                [ coaVis
                                    animationState
                                    selectedYear
                                    selectedCOA1
                                    (Dict.get selectedCOA1 countries)
                                    (coaPopulation countries selectedCOA1)
                                  <|
                                    Maybe.andThen (Dict.get selectedYear) <|
                                        Dict.get selectedCOA1 availableCOAs
                                , coaVis
                                    animationState
                                    selectedYear
                                    selectedCOA2
                                    (Dict.get selectedCOA2 countries)
                                    (coaPopulation countries selectedCOA2)
                                  <|
                                    Maybe.andThen (Dict.get selectedYear) <|
                                        Dict.get selectedCOA2 availableCOAs
                                ]
                            ]
                        , Introduction.introduction
                        ]
    }
