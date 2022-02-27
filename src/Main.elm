module Main exposing (main)

import Api exposing (..)
import Browser
import Browser.Events
import Browser.Navigation exposing (pushUrl, replaceUrl)
import Data
import Dict exposing (Dict)
import Html
    exposing
        ( Html
        , a
        , br
        , button
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
import Html.Events exposing (onClick, onInput)
import Html.Styled exposing (toUnstyled)
import Http exposing (get)
import Introduction
import List exposing (filter, head, map, range, reverse, tail)
import Maybe exposing (andThen, withDefault)
import Platform.Cmd
import Random as R exposing (Seed, initialSeed, int, step)
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
import Url exposing (Url)
import Url.Parser exposing ((</>), string)


main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlRequest = UrlChange
        , onUrlChange = UrlChange << Browser.Internal
        }


init : () -> Url -> Browser.Navigation.Key -> ( Model, Cmd Msg )
init _ url key =
    let
        ( coo, coa1, coa2 ) =
            withDefault defaultUrlTriple <| Url.Parser.parse urlTriple url
    in
    ( Ok
        { currentYear = Nothing
        , countries = Nothing
        , coo = coo
        , cooSelectState = Select.initState
        , coa1 = coa1
        , coa2 = coa2
        , coaSelect = Nothing
        , navigationKey = key
        , shortIntroductionVisible = True
        , longIntroductionVisible = False
        , infoFootprintsVisible = False
        , missingMigrants = ( Just False, initWait )
        }
    , Cmd.batch [ fetchCountries GotCountries, Task.perform StartupTime Time.now ]
    )


{-| This is the COO/COA1/COA2 combination, that is displayed on first page load, if nothing else is specified in the URL.
-}
defaultUrlTriple =
    ( "LKA", "FRA", "GBR" )


type alias Model =
    Result ApplicationError ApplicationState


type ApplicationError
    = CountriesLoadingFailed
    | AsylumDecisionsLoadingFailed


{-| About coaSelect: The outer Maybe is for the case if it isn't loaded yet. The inner Result is for no available data
-}
type alias ApplicationState =
    { currentYear : Maybe Int
    , navigationKey : Browser.Navigation.Key
    , shortIntroductionVisible : Bool
    , longIntroductionVisible : Bool
    , countries : Maybe (Dict CountryCode Country)
    , coo : CountryCode
    , cooSelectState : Select.State
    , coa1 : CountryCode
    , coa2 : CountryCode
    , coaSelect : Maybe (Result String COASelect)
    , infoFootprintsVisible : Bool
    , missingMigrants : MissingMigrantsState
    }


type alias MissingMigrantsState =
    ( Maybe Bool, AnimationState )


type alias COASelect =
    { availableCOAs : AvailableCOAs
    , coa1SelectState : Select.State
    , coa2SelectState : Select.State
    , year : Year
    , animationStates : ( AnimationState, AnimationState )
    , infoStates : ( InfoState, InfoState )
    }


{-|

  - Wait: Wait a few seconds before anything starts
  - FootprintsMoving: Move the footprints from right to left
  - Finished: The animation has stopped, we can cancel the subscription to animation frames

-}
type AnimationState
    = Wait Float
    | FootprintsMoving Float (List FootprintSteps)
    | Finished


{-| A list of positions, accompanied by frame count, that describes one
movement of an individual pair of footprints over time.
-}
type alias FootprintSteps =
    List ( Float, Point )


type alias Point =
    ( Float, Float )


{-| Initial state for AnimationState
-}
initWait =
    Wait 3000


type alias InfoState =
    { infoRecognizedVisible : Bool
    , infoComplementaryVisible : Bool
    , infoOtherwiseVisible : Bool
    , infoRejectedVisible : Bool
    }


{-| Initial state for InfoState
-}
initInfoState =
    { infoRecognizedVisible = False
    , infoComplementaryVisible = False
    , infoOtherwiseVisible = False
    , infoRejectedVisible = False
    }


type Msg
    = StartupTime Time.Posix
    | UpdateAnimation Float
    | ToggleFootprintsInfo
    | ToggleInfo ( InfoState, InfoState )
    | HideIntroduction
    | ToggleMissingMigrantsInfo
    | GotCountries (Result Http.Error (Dict CountryCode Country))
    | ChangeCoo (Select.Msg CountryCode)
    | GotAsylumDecisions (Result Http.Error AvailableCOAs)
    | ChangeCoa1 (Select.Msg CountryCode)
    | ChangeCoa2 (Select.Msg CountryCode)
    | ChangeYear Year
    | UrlChange Browser.UrlRequest


{-| Extract current COO/COA1/COA2 combination as URL from state.
-}
currentUrl : ApplicationState -> String
currentUrl { coo, coa1, coa2 } =
    "/" ++ coo ++ "/" ++ coa1 ++ "/" ++ coa2


{-| Parse COA/COA1/COA2 triple from an URL.
-}
urlTriple : Url.Parser.Parser (( String, String, String ) -> a) a
urlTriple =
    Url.Parser.map (\coo -> \coa1 -> \coa2 -> ( coo, coa1, coa2 )) <| string </> string </> string


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        noop =
            ( model, Cmd.none )
    in
    case model of
        Err _ ->
            noop

        Ok state ->
            case msg of
                StartupTime time ->
                    ( Ok { state | currentYear = Just <| Time.toYear Time.utc time }, Cmd.none )

                UpdateAnimation delta ->
                    case state.coaSelect of
                        Just (Ok coaS) ->
                            ( Ok
                                { state
                                    | missingMigrants =
                                        Tuple.mapSecond
                                            (updateMissingMigrantsAnimationState delta)
                                            state.missingMigrants
                                    , coaSelect =
                                        Just
                                            (Ok
                                                { coaS
                                                    | animationStates =
                                                        Tuple.mapBoth
                                                            (updateAnimationState
                                                                ( andThen
                                                                    (Dict.get state.coa1)
                                                                    state.countries
                                                                , andThen
                                                                    (Dict.get coaS.year)
                                                                  <|
                                                                    Dict.get state.coa1 coaS.availableCOAs
                                                                , coaS.year
                                                                )
                                                                delta
                                                            )
                                                            (updateAnimationState
                                                                ( andThen
                                                                    (Dict.get state.coa2)
                                                                    state.countries
                                                                , andThen
                                                                    (Dict.get coaS.year)
                                                                  <|
                                                                    Dict.get state.coa2 coaS.availableCOAs
                                                                , coaS.year
                                                                )
                                                                delta
                                                            )
                                                            coaS.animationStates
                                                }
                                            )
                                }
                            , Cmd.none
                            )

                        _ ->
                            noop

                GotCountries countriesResult ->
                    case countriesResult of
                        Err _ ->
                            ( Err CountriesLoadingFailed, Cmd.none )

                        Ok countries ->
                            ( Ok { state | countries = Just countries }
                            , fetchAsylumDecisions GotAsylumDecisions state.coo
                            )

                ChangeCoo selectedCooMsg ->
                    let
                        ( maybeAction, updatedSelectState, cmds ) =
                            Select.update selectedCooMsg state.cooSelectState

                        updatedCoo =
                            case maybeAction of
                                Just (Select.Select c) ->
                                    c

                                _ ->
                                    state.coo
                    in
                    if state.coo /= updatedCoo then
                        let
                            newState =
                                { state
                                    | cooSelectState = updatedSelectState
                                    , coo = updatedCoo
                                    , coaSelect = Nothing
                                }
                        in
                        ( Ok newState
                        , Cmd.batch
                            [ fetchAsylumDecisions GotAsylumDecisions updatedCoo
                            , Cmd.map ChangeCoo cmds
                            , replaceUrl newState.navigationKey <| currentUrl newState
                            ]
                        )

                    else
                        ( Ok { state | cooSelectState = updatedSelectState }
                        , Cmd.map ChangeCoo cmds
                        )

                GotAsylumDecisions asylumDecisionsResult ->
                    case asylumDecisionsResult of
                        Err _ ->
                            ( Err AsylumDecisionsLoadingFailed, Cmd.none )

                        Ok availableCOAs ->
                            let
                                fasCOAs =
                                    filteredAndSortedCOAs
                                        (Maybe.withDefault Dict.empty state.countries)
                                        availableCOAs

                                coa1 =
                                    if Dict.member state.coa1 availableCOAs then
                                        state.coa1

                                    else
                                        withDefault unknownCountryCode <|
                                            head <|
                                                map Tuple.first <|
                                                    fasCOAs

                                coa2 =
                                    if Dict.member state.coa2 availableCOAs then
                                        state.coa2

                                    else
                                        withDefault coa1 <|
                                            andThen List.head <|
                                                List.tail <|
                                                    map Tuple.first <|
                                                        fasCOAs

                                newState =
                                    { state
                                        | coa1 = coa1
                                        , coa2 = coa2
                                        , coaSelect =
                                            if List.isEmpty fasCOAs then
                                                Just <| Err "empty data"

                                            else
                                                Just <|
                                                    Ok
                                                        { availableCOAs = availableCOAs
                                                        , coa1SelectState = Select.initState
                                                        , coa2SelectState = Select.initState
                                                        , year = "2000"
                                                        , animationStates = ( initWait, initWait )
                                                        , infoStates = ( initInfoState, initInfoState )
                                                        }
                                    }
                            in
                            ( Ok newState
                            , pushUrl newState.navigationKey <| currentUrl newState
                            )

                ChangeCoa1 selectedCoa1Msg ->
                    case state.coaSelect of
                        Just (Ok coaS) ->
                            let
                                ( maybeAction, updatedSelectState, cmds ) =
                                    Select.update selectedCoa1Msg coaS.coa1SelectState

                                updatedCoa1 =
                                    case maybeAction of
                                        Just (Select.Select c) ->
                                            c

                                        _ ->
                                            state.coa1

                                newState =
                                    { state
                                        | coa1 = updatedCoa1
                                        , coaSelect =
                                            Just
                                                (Ok
                                                    { coaS
                                                        | coa1SelectState = updatedSelectState
                                                        , animationStates =
                                                            Tuple.mapFirst
                                                                (if state.coa1 /= updatedCoa1 then
                                                                    always initWait

                                                                 else
                                                                    identity
                                                                )
                                                                coaS.animationStates
                                                    }
                                                )
                                    }
                            in
                            ( Ok newState
                            , Cmd.batch
                                [ Cmd.map ChangeCoa1 cmds
                                , pushUrl newState.navigationKey <| currentUrl newState
                                ]
                            )

                        _ ->
                            noop

                ChangeCoa2 selectedCoa2Msg ->
                    case state.coaSelect of
                        Just (Ok coaS) ->
                            let
                                ( maybeAction, updatedSelectState, cmds ) =
                                    Select.update selectedCoa2Msg coaS.coa2SelectState

                                updatedCoa2 =
                                    case maybeAction of
                                        Just (Select.Select c) ->
                                            c

                                        _ ->
                                            state.coa2

                                newState =
                                    { state
                                        | coa2 = updatedCoa2
                                        , coaSelect =
                                            Just
                                                (Ok
                                                    { coaS
                                                        | coa2SelectState = updatedSelectState
                                                        , animationStates =
                                                            Tuple.mapSecond
                                                                (if state.coa2 /= updatedCoa2 then
                                                                    always initWait

                                                                 else
                                                                    identity
                                                                )
                                                                coaS.animationStates
                                                    }
                                                )
                                    }
                            in
                            ( Ok newState
                            , Cmd.batch
                                [ Cmd.map ChangeCoa1 cmds
                                , pushUrl newState.navigationKey <| currentUrl newState
                                ]
                            )

                        _ ->
                            noop

                ChangeYear year ->
                    case state.coaSelect of
                        Just (Ok coaS) ->
                            ( Ok
                                { state
                                    | coaSelect =
                                        Just
                                            (Ok
                                                { coaS
                                                    | year = year
                                                    , animationStates = ( initWait, initWait )
                                                }
                                            )
                                }
                            , Cmd.none
                            )

                        _ ->
                            noop

                UrlChange urlRequest ->
                    case urlRequest of
                        Browser.External url ->
                            ( model, Browser.Navigation.load url )

                        Browser.Internal url ->
                            case state.coaSelect of
                                Just coaS ->
                                    let
                                        ( coo, coa1, coa2 ) =
                                            withDefault defaultUrlTriple <| Url.Parser.parse urlTriple url

                                        newState =
                                            { state | coo = coo, coa1 = coa1, coa2 = coa2 }

                                        newUrl =
                                            { url | path = currentUrl newState }
                                    in
                                    if url == newUrl then
                                        noop

                                    else
                                        ( Ok newState
                                        , pushUrl newState.navigationKey <| Url.toString newUrl
                                        )

                                _ ->
                                    noop

                ToggleFootprintsInfo ->
                    ( Ok { state | infoFootprintsVisible = not state.infoFootprintsVisible }, Cmd.none )

                ToggleMissingMigrantsInfo ->
                    let
                        newVisibility =
                            case Tuple.first state.missingMigrants of
                                Just False ->
                                    Just True

                                Just True ->
                                    Nothing

                                _ ->
                                    Nothing
                    in
                    ( Ok { state | missingMigrants = ( newVisibility, Finished ) }, Cmd.none )

                HideIntroduction ->
                    ( Ok { state | shortIntroductionVisible = False }, Cmd.none )

                ToggleInfo toggledInfoStates ->
                    case state.coaSelect of
                        Just (Ok coaS) ->
                            ( Ok
                                { state
                                    | coaSelect =
                                        Just
                                            (Ok
                                                { coaS
                                                    | infoStates = toggledInfoStates
                                                }
                                            )
                                }
                            , Cmd.none
                            )

                        _ ->
                            noop


{-| Total length of the animation of footprints in ms.
-}
footprintAnimationLength =
    3000


{-| We can't really compute the whole animation sequence for every frame, so
we have to hold it as state. Here is this state generated once for a given
animation for each footprint.
-}
prepareFootprintMovement : Float -> Seed -> FootprintSteps
prepareFootprintMovement ratio initSeed =
    reverse <|
        let
            move : Float -> Seed -> Point -> ( Float, Float, Float ) -> FootprintSteps
            move frame seed0 ( x, y ) ( direction, length, dt ) =
                if frame > footprintAnimationLength then
                    []

                else
                    let
                        ( newDirection, seed1 ) =
                            R.step
                                (R.map ((+) direction >> (*) 0.5) <|
                                    R.float
                                        (-0.2 * 2 * pi)
                                        (0.2 * 2 * pi)
                                )
                                seed0

                        ( newLength, seed2 ) =
                            R.step
                                (R.map ((+) length) <| R.float -0.5 1.5)
                                seed1

                        ( newDt, seed3 ) =
                            R.step
                                (R.map ((+) dt) <| R.float -30 30)
                                seed2

                        newCursor =
                            ( x + newLength * cos newDirection, y + newLength * sin newDirection )
                    in
                    ( frame + newDt, ( x, y ) )
                        :: move
                            (frame + newDt)
                            seed3
                            newCursor
                            ( newDirection, newLength, max 10 newDt )
        in
        move (ratio * 1000) initSeed ( 0, 0 ) ( 0, 20, 100 )


{-| Initial state for AnimationState
-}
initFootprintsMoving : ( Maybe Country, Maybe AsylumDecisions, Year ) -> AnimationState
initFootprintsMoving ( maybeCountry, maybeAsylumDecisions, year ) =
    let
        footprintCount =
            case ( coaPopulation maybeCountry, maybeAsylumDecisions ) of
                ( Ok population, Just aD ) ->
                    aD.total * perCapitaUnit // population

                _ ->
                    0

        yearInt =
            withDefault 2000 <| String.toInt year

        -- a ratio that tells us about in which place a footprint is
        ratio i =
            (toFloat footprintCount - toFloat i) / toFloat footprintCount
    in
    if footprintCount == 0 then
        Finished

    else
        FootprintsMoving footprintAnimationLength <|
            map (\i -> prepareFootprintMovement (ratio i) <| initialSeed <| yearInt + i) <|
                List.range 1 footprintCount


{-| Clean FootprintSteps of outdated animation keyframes. We assume
FootprintSteps to be a List ordered by animation frame.
-}
cleanOutdatedFootprintSteps : Float -> FootprintSteps -> FootprintSteps
cleanOutdatedFootprintSteps t fpSteps =
    case fpSteps of
        [] ->
            []

        ( maxFrame, point ) :: remainingSteps ->
            if maxFrame < t then
                fpSteps

            else
                cleanOutdatedFootprintSteps t remainingSteps


updateAnimationState : ( Maybe Country, Maybe AsylumDecisions, Year ) -> Float -> AnimationState -> AnimationState
updateAnimationState fpInfo delta aS =
    case aS of
        Wait t ->
            if t - delta <= 0 then
                initFootprintsMoving fpInfo

            else
                Wait <| t - delta

        FootprintsMoving t footprintState ->
            if t - delta <= 0 then
                Finished

            else
                FootprintsMoving (t - delta) <| map (cleanOutdatedFootprintSteps (t - delta)) footprintState

        Finished ->
            Finished


{-| The update for the missing migrants footstep is a bit different, hence the duplicated logic.
-}
updateMissingMigrantsAnimationState : Float -> AnimationState -> AnimationState
updateMissingMigrantsAnimationState delta aS =
    case aS of
        Wait t ->
            if t - delta <= 0 then
                FootprintsMoving 4000 <|
                    List.singleton <|
                        prepareFootprintMovement 0 <|
                            initialSeed 42

            else
                Wait <| t - delta

        FootprintsMoving t footprintState ->
            if t - delta <= 0 then
                Finished

            else
                FootprintsMoving (t - delta) <| map (cleanOutdatedFootprintSteps (t - delta)) footprintState

        Finished ->
            Finished


{-| Elm subcriptions that we want to subcribe to during runtime. We only need
time spent since last animation draw IF there is actually an animation running.
-}
subscriptions : Model -> Sub Msg
subscriptions model =
    case model of
        Err _ ->
            Sub.none

        Ok state ->
            case ( state.coaSelect, state.shortIntroductionVisible ) of
                ( Just (Ok coaS), False ) ->
                    case ( Tuple.second state.missingMigrants, coaS.animationStates ) of
                        ( Finished, ( Finished, Finished ) ) ->
                            Sub.none

                        _ ->
                            Browser.Events.onAnimationFrameDelta UpdateAnimation

                _ ->
                    Sub.none


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


{-| Helper function for a country select
-}
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


{-| One item that is selectable in a cooSelect/coaSelect
-}
menuItem : ( CountryCode, Country ) -> Select.MenuItem CountryCode
menuItem ( cc, country ) =
    { item = cc, label = country.name }


{-| Select element for a country of origin.
-}
cooSelect : Dict CountryCode Country -> CountryCode -> Select.State -> Html Msg
cooSelect countries selectedCountry =
    countrySelectElement "selectCOO"
        ChangeCoo
        (Dict.toList countries)
        (Maybe.map (\c -> menuItem ( selectedCountry, c )) <| Dict.get selectedCountry countries)


{-| Select element for a country of asylum.
-}
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


temporalSvg : Year -> ( CountryCode, CountryCode ) -> AvailableCOAs -> Html Msg
temporalSvg selectedYear ( coa1, coa2 ) availableCOAs =
    let
        years =
            List.range 2000 2021

        blockWidth =
            1 / (toFloat <| List.length years)

        decisionsData coa getter year =
            Maybe.map toFloat <|
                andThen getter <|
                    andThen (Dict.get <| fromInt year) <|
                        Dict.get coa availableCOAs

        value coa year =
            Maybe.map2
                (/)
                (decisionsData coa .other year)
                (decisionsData coa (.total >> Just) year)

        chart coa yBase =
            List.filterMap identity <|
                List.indexedMap
                    (\i year ->
                        andThen
                            (\v ->
                                Just <|
                                    rect
                                        [ x <| fromFloat <| toFloat i * blockWidth
                                        , y <| fromFloat <| yBase - v
                                        , width <| fromFloat blockWidth
                                        , height <| fromFloat v
                                        , stroke "none"
                                        , fill <|
                                            if fromInt year /= selectedYear then
                                                -- the tone from our coaSvg for complementary protection
                                                "#b7b7b7"

                                            else
                                                -- a slightly redder tone for the selected year
                                                "#fa6363"
                                        ]
                                        []
                            )
                        <|
                            value coa year
                    )
                    years
    in
    svg
        [ width "100"
        , height "5em"
        , viewBox "0 0 1 2"
        , style "width: 100%;"
        , preserveAspectRatio "none"
        ]
    <|
        chart coa1 1.0
            ++ chart coa2 2.0


yearOption : String -> Html Msg
yearOption year =
    option [ value year ] [ text year ]


{-| The HTML input element that makes the year selectable
-}
yearInput : Int -> Html Msg
yearInput currentYear =
    let
        maxYear =
            currentYear - 1
    in
    input
        [ type_ "range"
        , onInput ChangeYear
        , HA.min "2000"
        , HA.max <| fromInt maxYear
        , style "width: 99%;"
        ]
    <|
        map yearOption <|
            map fromInt <|
                range 2000 maxYear


footprint1 : String -> String -> Svg Msg
footprint1 idStr color =
    symbol [ id idStr ]
        [ path [ stroke "none", fill color, d "M 2.12064 2.21568 C 2.48736 2.20482 2.7805 2.19638 3.00005 2.19035 C 3.46328 2.17708 3.29801 -0.00515408 2.59593 0.0696378 C 2.00966 0.13116 1.65742 2.23016 2.12064 2.21568 Z" ] []
        , path [ stroke "none", fill color, d "M 2.10737 2.47625 C 2.25937 2.47142 2.94335 2.44971 3.01935 2.44729 C 3.17135 2.44247 3.09052 3.19521 3.01211 3.38461 C 2.9325 3.58003 2.22559 3.51489 2.14959 3.42321 C 2.03138 3.27966 1.95538 2.48107 2.10737 2.47625 Z" ] []
        , path [ stroke "none", fill color, d "M 0.309957 3.4027 C 0.676678 3.38461 0.969814 3.37134 1.19057 3.36048 C 1.655 3.33756 1.32809 1.1698 0.629631 1.25786 C 0.0445662 1.33024 -0.154477 3.42441 0.309957 3.4027 Z" ] []
        , path [ stroke "none", fill color, d "M 0.303925 3.67895 C 0.455921 3.67171 1.14111 3.63914 1.21711 3.63552 C 1.36911 3.62828 1.33895 4.36534 1.27501 4.55232 C 1.20867 4.74533 0.494524 4.69226 0.412494 4.60419 C 0.282211 4.46426 0.151929 3.68619 0.303925 3.67895 Z" ] []
        ]


footprint2 : String -> String -> Svg Msg
footprint2 idStr color =
    symbol [ id idStr ]
        [ path [ stroke "none", fill color, d "M 2.57663 2.75491 C 2.90113 2.68856 3.16049 2.63669 3.35591 2.59688 C 3.76606 2.51364 3.11948 0.172175 2.50908 0.356742 C 1.9988 0.511151 2.16528 2.83814 2.57663 2.75491 Z" ] []
        , path [ stroke "none", fill color, d "M 2.62006 3.00582 C 2.75517 2.97808 3.36074 2.85503 3.42829 2.84176 C 3.5634 2.81402 3.62854 3.48232 3.59477 3.65965 C 3.55978 3.8418 2.91681 3.89247 2.83237 3.82491 C 2.69968 3.71634 2.48495 3.03236 2.62006 3.00582 Z" ] []
        , path [ stroke "none", fill color, d "M 0.389574 3.1168 C 0.714074 3.18315 0.973433 3.23623 1.16765 3.27604 C 1.5778 3.35927 1.88903 0.993679 1.25451 0.924919 C 0.727343 0.867015 -0.0205751 3.03357 0.389574 3.1168 Z" ] []
        , path [ stroke "none", fill color, d "M 0.309957 3.4027 C 0.445064 3.43045 1.05064 3.55349 1.11819 3.56797 C 1.2533 3.59571 1.05184 4.23627 0.951719 4.38585 C 0.847975 4.54026 0.236371 4.33519 0.185706 4.23989 C 0.104882 4.08789 0.174849 3.37495 0.309957 3.4027 Z" ] []
        ]


footprint3 : String -> String -> Svg Msg
footprint3 idStr color =
    symbol [ id idStr ]
        [ path [ stroke "none", fill color, d "M 2.36046 3.01764 C 2.68134 3.10208 2.93708 3.16843 3.12889 3.2191 C 3.53421 3.32525 3.94315 1.09959 3.31586 0.992231 C 2.79232 0.902964 1.95514 2.91149 2.36046 3.01764 Z" ] []
        , path [ stroke "none", fill color, d "M 2.2917 3.16723 C 2.42439 3.20221 3.02273 3.35903 3.08908 3.37713 C 3.22177 3.41211 2.98654 4.0406 2.87676 4.18415 C 2.76458 4.33253 2.16504 4.09368 2.1204 3.99597 C 2.04923 3.84035 2.159 3.13224 2.2917 3.16723 Z" ] []
        , path [ stroke "none", fill color, d "M 0.739165 3.15396 C 1.06367 3.08761 1.32302 3.03453 1.51724 2.99472 C 1.92739 2.91028 1.30975 0.732872 0.69815 0.915027 C 0.19029 1.06702 0.329016 3.2384 0.739165 3.15396 Z" ] []
        , path [ stroke "none", fill color, d "M 0.777768 3.30233 C 0.912875 3.27459 1.51845 3.15034 1.586 3.13707 C 1.72111 3.10932 1.78746 3.77762 1.75368 3.95495 C 1.7187 4.13831 1.07573 4.18898 0.992492 4.12143 C 0.858591 4.01286 0.643866 3.32887 0.777768 3.30233 Z" ] []
        ]


footprint4 : String -> String -> Svg Msg
footprint4 idStr color =
    symbol [ id idStr ]
        [ path [ stroke "none", fill color, d "M 2.2917 3.16723 C 2.61861 3.22151 2.87918 3.26494 3.07581 3.29751 C 3.48957 3.36627 3.69224 1.11166 3.05771 1.0622 C 2.52814 1.02118 1.87914 3.09847 2.2917 3.16723 Z" ] []
        , path [ stroke "none", fill color, d "M 2.21691 3.57617 C 2.35202 3.59909 2.96241 3.70042 3.02997 3.71128 C 3.16507 3.7342 2.98775 4.38078 2.89245 4.53399 C 2.79473 4.69201 2.17589 4.50986 2.12161 4.41577 C 2.03717 4.2686 2.0818 3.55325 2.21691 3.57617 Z" ] []
        , path [ stroke "none", fill color, d "M 0.620946 2.44223 C 0.94424 2.36864 1.20239 2.31074 1.3954 2.26611 C 1.80435 2.17322 1.13966 0.0090805 0.532885 0.204504 C 0.0274365 0.366151 0.212003 2.53391 0.620946 2.44223 Z" ] []
        , path [ stroke "none", fill color, d "M 0.690912 2.85238 C 0.824814 2.82222 1.42797 2.6847 1.49553 2.67022 C 1.62943 2.64006 1.71025 3.30595 1.68009 3.48449 C 1.64873 3.66785 1.00817 3.73299 0.922526 3.66664 C 0.787418 3.56169 0.557011 2.88253 0.690912 2.85238 Z" ] []
        ]


infoboxStyle =
    "width: 500px; "
        ++ "position: absolute; "
        ++ "overflow: visible; "
        ++ "text-align: left;"
        ++ "background: white;"
        ++ "border: 0.1em solid grey; "
        ++ "border-radius: 0.5em; "
        ++ "padding: 1em; "
        ++ "z-index: 2; "
        ++ "font-size: 75%; "


infobuttonStyle visible =
    "border: none; "
        ++ (if visible then
                "font-weight: bolder; "
                    ++ "background: lightgrey; "
                    ++ "border-radius: 0.3em; "

            else
                "background: none; "
           )


{-| SVG containing footprints for one decisison category
-}
footprintDiagram : AnimationState -> Seed -> Simplex.PermutationTable -> Bool -> Int -> ( Int, Int ) -> ( Float, Float ) -> List (Svg Msg)
footprintDiagram animationState seed permTable elevatedRow count ( currentColumn, maxColumns ) ( xPos, yPerc ) =
    let
        -- base distance
        dy =
            6.5

        dx =
            sqrt 3 * 0.5 * dy

        noiseStrength =
            3.0

        -- noise function we use to move the footprints around
        noise =
            (*) noiseStrength
                >> Simplex.fractal2d
                    { scale = 0.5
                    , steps = 7
                    , stepSize = 2.0
                    , persistence = 2.0
                    }
                    permTable

        capped =
            currentColumn >= maxColumns
    in
    case ( animationState, count, capped ) of
        ( Wait _, _, _ ) ->
            []

        ( _, 0, _ ) ->
            []

        ( Finished, _, True ) ->
            [ circle [ cx <| fromFloat <| xPos + dx - 4.0, cy "97", r "0.75", fill "black" ] []
            , circle [ cx <| fromFloat <| xPos + dx - 2.0, cy "97", r "0.75", fill "black" ] []
            , circle [ cx <| fromFloat <| xPos + dx + 0.0, cy "97", r "0.75", fill "black" ] []
            ]

        ( _, _, True ) ->
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

                -- noise component, that causes the footprints to be placed irregularly
                ( noiseX, noiseY ) =
                    ( noise xPos yPos, noise yPos (xPos + 1000) )

                -- animation component that is dependent on the state of the animation
                ( ( animX, animY ), nextAnimationState ) =
                    case animationState of
                        FootprintsMoving t fpSteps ->
                            ( withDefault ( 0, 0 ) <|
                                Maybe.map Tuple.second <|
                                    andThen head <|
                                        head fpSteps
                            , FootprintsMoving t <| withDefault [] <| tail fpSteps
                            )

                        aS ->
                            ( ( 0, 0 ), aS )
            in
            use
                [ attribute "href"
                    (String.append "#fs" <| fromInt symbolIndex)
                , x <| fromFloat <| xPos + noiseX + animX
                , y <| fromFloat <| yPos + noiseY + animY
                ]
                []
                :: (if yPerc > 5 then
                        footprintDiagram
                            nextAnimationState
                            nextSeed
                            permTable
                            elevatedRow
                            (count - 1)
                            ( currentColumn, maxColumns )
                            ( xPos, yPerc - dy )

                    else
                        footprintDiagram
                            nextAnimationState
                            nextSeed
                            permTable
                            (not elevatedRow)
                            (count - 1)
                            ( currentColumn + 1, maxColumns )
                            ( xPos + dx, 95 )
                   )


{-| The legend for an individual category in the COA chart.
With enough space it includes a percentage number, the name of the category and
an info icon allowing to open a larger explanation div.
-}
categoryLegend : Float -> Float -> String -> Float -> Msg -> Bool -> List (Html Msg) -> Html Msg
categoryLegend xPos width categoryName linewrapHeuristic msg infoVisible explanation =
    let
        infobutton =
            button
                [ class "info_button"
                , onClick msg
                , style <| infobuttonStyle infoVisible
                ]
                [ text "ⓘ" ]
    in
    div []
        (if width < 1.5 then
            []

         else
            [ div
                [ style <|
                    "overflow: hidden; "
                        ++ "text-overflow: ellipsis; "
                        ++ "position: absolute; "
                        ++ "top: 80%; "
                        ++ "text-align: center; "
                        ++ ("left: " ++ fromFloat xPos ++ "%; ")
                        ++ ("width: " ++ fromFloat width ++ "%; ")
                        ++ "z-index: 1; "
                ]
                (if width < 4 then
                    [ infobutton ]

                 else if width < linewrapHeuristic then
                    [ S.text <| (fromInt <| round width) ++ "%"
                    , infobutton
                    ]

                 else
                    [ S.text <| (fromInt <| round width) ++ "% " ++ categoryName
                    , infobutton
                    ]
                )
            , if infoVisible then
                div [ style <| infoboxStyle ++ "top: 5em; " ++ ("left: " ++ fromFloat xPos ++ "%; ") ]
                    ([ if width < linewrapHeuristic then
                        p [] [ text <| (fromInt <| round width) ++ "% " ++ categoryName ]

                       else
                        text ""
                     ]
                        ++ explanation
                    )

              else
                text ""
            ]
        )


missingMigrantsVis : MissingMigrantsState -> List (Svg Msg)
missingMigrantsVis missingMigrantsState =
    case missingMigrantsState of
        ( Just _, FootprintsMoving t fpSteps ) ->
            let
                footprint ( animX, animY ) idStr =
                    use
                        [ attribute "href" idStr
                        , x <| fromFloat <| 10 + animX
                        , y <| fromFloat <| 130 + animY
                        ]
                        []
            in
            List.map2
                footprint
                (map Tuple.second <| withDefault [] <| head fpSteps)
                [ "#fsmm1", "#fsmm2", "#fsmm3", "#fsmm4", "#fsmm5" ]

        _ ->
            []


missingMigrantsInfobox : MissingMigrantsState -> Html Msg
missingMigrantsInfobox missingMigrantsState =
    case missingMigrantsState of
        ( Just visible, Finished ) ->
            div
                [ style <|
                    "position: absolute; "
                        ++ "margin-top: 27em; "
                        ++ "margin-left: 2em; "
                        ++ "z-index: 3; "
                ]
                [ button [ class "info_button", onClick ToggleMissingMigrantsInfo, style <| infobuttonStyle visible ] [ text "ⓘ" ]
                , if visible then
                    div [ style <| infoboxStyle ]
                        [ h1 [ infoboxH1Style ]
                            [ text "Missing Migrants" ]
                        , text "missing migrants"
                        ]

                  else
                    text ""
                ]

        _ ->
            text ""


{-| Produces three different elements for each decision category:

  - SVG consisting of the rectangle
  - SVG consisting of footprints
  - HTML that describes the category

-}
barElement :
    Maybe MissingMigrantsState
    -> AnimationState
    -> Bool
    -> ( InfoState, InfoState )
    -> Int
    -> Int
    -> String
    -> List (Html Msg)
    -> Float
    -> String
    -> Int
    -> Int
    -> ( Svg Msg, Svg Msg, Html Msg )
barElement maybeMissingMigrants animationState infoVisible toggledInfoState dividend position categoryName explanation linewrapHeuristic color total population =
    let
        xPos =
            100 * (toFloat position / toFloat total)

        width =
            100 * (toFloat dividend / toFloat total)

        footprintCount =
            dividend * perCapitaUnit // population

        -- We assume that at maximum 80 footprint columns fit into the chart.
        maxColumnCount =
            (round <| 80 * 0.01 * width) - 2

        -- Our share of the precomputed footprint positions for this category
        animationStateShare =
            case animationState of
                FootprintsMoving t fpSteps ->
                    FootprintsMoving t <| List.drop (position * perCapitaUnit // population) fpSteps

                aS ->
                    aS
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
        , SA.width (fromFloat (100 - xPos) ++ "%")
        , preserveAspectRatio "xMinYMin meet"
        ]
        ([ S.title []
            [ text <|
                fromInt footprintCount
                    ++ " decisions/"
                    ++ perCapitaUnitString
                    ++ " inhabitants, "
            ]
         ]
            ++ (withDefault [] <| Maybe.map missingMigrantsVis maybeMissingMigrants)
            ++ footprintDiagram
                animationStateShare
                (initialSeed <| population + position)
                (Simplex.permutationTableFromInt <| population + position)
                False
                footprintCount
                ( 0, maxColumnCount )
                ( 5, 95 )
        )
    , categoryLegend
        xPos
        width
        categoryName
        linewrapHeuristic
        (ToggleInfo toggledInfoState)
        infoVisible
        explanation
    )


{-| The SVG component of a COA chart
-}
coaSvg : Maybe MissingMigrantsState -> AnimationState -> InfoState -> Bool -> Int -> AsylumDecisions -> Html Msg
coaSvg maybeMissingMigrants animationState infoState isCOA1 population ad =
    let
        toggledInfoStates iS =
            if isCOA1 then
                ( iS, initInfoState )

            else
                ( initInfoState, iS )

        barElements =
            map (\f -> f ad.total population)
                [ barElement
                    maybeMissingMigrants
                    animationState
                    infoState.infoRecognizedVisible
                    (toggledInfoStates { initInfoState | infoRecognizedVisible = not infoState.infoRecognizedVisible })
                    (withDefault 0 ad.recognized)
                    0
                    "recognized"
                    [ text "Asylum claims that have been recognized in the chosen period of time. The applicants are now recognized refugees under UNHCR´s mandate." ]
                    13.5
                    "#a8a8a8"
                , barElement
                    Nothing
                    animationState
                    infoState.infoComplementaryVisible
                    (toggledInfoStates { initInfoState | infoComplementaryVisible = not infoState.infoComplementaryVisible })
                    (withDefault 0 ad.other)
                    (withDefault 0 ad.recognized)
                    "complementary protection"
                    [ text """
                    Country-specific forms of complementary or subsidiary protection for people that do not fall under other definitions of refugee, but still are in need of protection. What is included in these kinds of protection can widely differ, from protection from deportation to the full rights a refugee status entails. In the EU asylum system, a "third country national or stateless person" can be granted subsidiary protection if they do not qualify as a refugee but on return to their country of origin are in danger of "serious harm" (Expert Group on Refugee and Internally Displaced Persons Statistics 2018, 23). In Germany, major differences between a refugee status and subsidiary protection are: with subsidiary protection, family reunification is restricted. Moreover, a residence permit is issued only for one year and can be prolonged for two years at a time whereas with a refugee status, a residence permit directly is issued for three years with the chance of extension.
                    """ ]
                    27.7
                    "#b7b7b7"
                , barElement
                    Nothing
                    animationState
                    infoState.infoOtherwiseVisible
                    (toggledInfoStates { initInfoState | infoOtherwiseVisible = not infoState.infoOtherwiseVisible })
                    (withDefault 0 ad.closed)
                    (withDefault 0 ad.recognized + withDefault 0 ad.other)
                    "otherwise closed"
                    [ text "Asylum applications closed without a substantive decision (neither recognized nor rejected). Reasons for that can be e.g. withdrawal, inadmissibility, abandonment, death, Dublin II  procedure (Europe only), etc." ]
                    17.7
                    "#cecece"
                , barElement
                    Nothing
                    animationState
                    infoState.infoRejectedVisible
                    (toggledInfoStates { initInfoState | infoRejectedVisible = not infoState.infoRejectedVisible })
                    (withDefault 0 ad.rejected)
                    (withDefault 0 ad.recognized + withDefault 0 ad.other + withDefault 0 ad.closed)
                    "rejected"
                    [ text "Asylum claims that have been rejected in the chosen period of time." ]
                    11.5
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
            ([ footprint1 "fs1" "black"
             , footprint2 "fs2" "black"
             , footprint3 "fs3" "black"
             , footprint4 "fs4" "black"
             , footprint1 "fsmm1" "#c3c3e1"
             , footprint2 "fsmm2" "#9d9ee3"
             , footprint3 "fsmm3" "#7071e1"
             , footprint4 "fsmm4" "#4244df"
             , footprint1 "fsmm5" "#0002d5"
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
        , div
            [ style <|
                "position: relative;"
                    ++ " width: 100%;"
                    ++ " margin-bottom: 3em;"
                    ++ " top: -30em;"
            ]
          <|
            map legend barElements
        ]


{-| A complete chart for one COA.
-}
coaVis : Maybe MissingMigrantsState -> AnimationState -> InfoState -> Bool -> Year -> CountryCode -> Maybe Country -> Maybe AsylumDecisions -> Html Msg
coaVis maybeMissingMigrants animationState infoState isCOA1 year countryCode country maybeAsylumDecisions =
    div [ style <| "margin-bottom: 5em; " ++ "text-align: center; " ++ "margin-top: 2.5em; " ]
        ([ h2
            [ title <|
                String.append ("UNHCR: " ++ countryCode) <|
                    withDefault "" <|
                        Maybe.map (.iso >> String.append ", ISO: ") country
            , style <| "margin-bottom: 0.5em; "
            ]
            [ text <| withDefault "Unkown country name!" <| Maybe.map .name country ]
         ]
            ++ (case coaPopulation country of
                    Err e ->
                        [ text e ]

                    Ok population ->
                        case maybeAsylumDecisions of
                            Nothing ->
                                [ div [ style "margin-bottom: 0.5em; " ] [ text "No UNHCR data available." ]
                                , div [ style "height: 15em; " ] []
                                ]

                            Just ad ->
                                [ div
                                    [ style <|
                                        "margin-bottom: 0.5em; "
                                            ++ (if animationState == Finished then
                                                    "animation-name: blendin; animation-duration: 3s"

                                                else
                                                    "opacity: 0;"
                                               )
                                    ]
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
                                    ]
                                , coaSvg maybeMissingMigrants animationState infoState isCOA1 population ad
                                ]
               )
        )


infoboxH1Style : S.Attribute msg
infoboxH1Style =
    style "margin-bottom: unset; font-size: 100%;"


{-| The legend that explains what one footprint symbolises
-}
footprintLegend : Bool -> Html Msg
footprintLegend infoFootprintsVisible =
    div
        [ style <|
            "padding-left: 1em;"
                ++ " margin-top: 2em;"
                ++ " margin-bottom: 2em"
        ]
        ([ svg
            [ width "1em"
            , height "1em"
            , viewBox "0 0 5 5"
            ]
            [ footprint1 "fs1" "black"
            , use
                [ attribute "href" "#fs1"
                , y "0"
                , x "0"
                ]
                []
            ]
         , text <| "1 decision per " ++ perCapitaUnitString ++ " inhabitants"
         , button [ class "info_button", onClick ToggleFootprintsInfo, style <| infobuttonStyle infoFootprintsVisible ] [ text "ⓘ" ]
         ]
            ++ (if infoFootprintsVisible then
                    [ div [ style <| infoboxStyle ]
                        [ h1 [ infoboxH1Style ]
                            [ text "One decision does not equal one person!" ]
                        , text """
To learn more, read "What the web app cannot show you" in the introduction.
"""
                        ]
                    ]

                else
                    []
               )
        )


{-| Granularity in which we calculate asylum decision count in relation to population of the COA
-}
perCapitaUnit =
    500000


perCapitaUnitString =
    "500,000"


coaPopulation : Maybe Country -> Result String Int
coaPopulation maybeCountry =
    case maybeCountry of
        Nothing ->
            Err <| "Country not available."

        Just country ->
            Result.fromMaybe "No population data available." <|
                (Dict.get country.iso >> andThen (Dict.get 2018)) Data.population


{-| The complete menu bar to the left
-}
menubar : List (Html Msg) -> Html Msg
menubar html =
    div
        [ id "menubar"
        , style <|
            "float: left;"
                ++ " width: 24em;"
                ++ " margin-left: 3em;"
                ++ " margin-right: 8em;"
                ++ " overflow-y: scroll;"
                ++ " height: 100vh;"
                -- no scrollbar for Firefox
                ++ " scrollbar-width: none;"
                -- no scrollbar for IE, Edge
                ++ " -ms-overflow-style: none;"
        ]
        ([ h1 [] [ text appTitle ] ]
            ++ html
        )


appTitle =
    "How European States Decide on Asylum"


view : Model -> Browser.Document Msg
view model =
    { title = appTitle
    , body =
        case model of
            Err CountriesLoadingFailed ->
                [ text "An error occured while fetching the countries!" ]

            Err AsylumDecisionsLoadingFailed ->
                [ text "An error occured while fetching the asylum decisions!" ]

            Ok state ->
                [ div []
                    ([ menubar
                        (if state.shortIntroductionVisible then
                            [ Introduction.introduction HideIntroduction ]

                         else
                            case ( state.currentYear, state.countries ) of
                                ( Just currentYear, Just countries ) ->
                                    [ cooSelect
                                        countries
                                        state.coo
                                        state.cooSelectState
                                    ]
                                        ++ (case state.coaSelect of
                                                Nothing ->
                                                    [ text "loading..." ]

                                                Just coaSResult ->
                                                    case coaSResult of
                                                        Err _ ->
                                                            [ text <|
                                                                (String.append "Unfortunately there is no data available for " <|
                                                                    .name <|
                                                                        withDefault unknownCountry <|
                                                                            Dict.get state.coo countries
                                                                )
                                                                    ++ " yet."
                                                            ]

                                                        Ok coaS ->
                                                            [ br [] []
                                                            , coaSelect
                                                                countries
                                                                coaS.availableCOAs
                                                                state.coa1
                                                                coaS.coa1SelectState
                                                                state.coa2
                                                                coaS.coa2SelectState
                                                            , footprintLegend state.infoFootprintsVisible
                                                            , div [ style "height: 5em;" ]
                                                                [ temporalSvg
                                                                    coaS.year
                                                                    ( state.coa1, state.coa2 )
                                                                    coaS.availableCOAs
                                                                , yearInput currentYear
                                                                , p [ style "font-size: 4em; margin-top: 0;" ] [ text coaS.year ]
                                                                ]
                                                            ]
                                           )

                                _ ->
                                    [ text "Loading countries..."
                                    ]
                        )
                     ]
                        ++ (case ( state.countries, state.coaSelect ) of
                                ( Just countries, Just (Ok coaS) ) ->
                                    [ div
                                        [ style <|
                                            "float: left;"
                                                ++ " width: 60%;"
                                                ++ " margin-left: 3em;"
                                        ]
                                        [ missingMigrantsInfobox state.missingMigrants
                                        , coaVis
                                            (Just state.missingMigrants)
                                            (Tuple.first coaS.animationStates)
                                            (Tuple.first coaS.infoStates)
                                            True
                                            coaS.year
                                            state.coa1
                                            (Dict.get state.coa1 countries)
                                          <|
                                            andThen (Dict.get coaS.year) <|
                                                Dict.get state.coa1 coaS.availableCOAs
                                        , coaVis
                                            Nothing
                                            (Tuple.second coaS.animationStates)
                                            (Tuple.second coaS.infoStates)
                                            False
                                            coaS.year
                                            state.coa2
                                            (Dict.get state.coa2 countries)
                                          <|
                                            andThen (Dict.get coaS.year) <|
                                                Dict.get state.coa2 coaS.availableCOAs
                                        ]
                                    ]

                                _ ->
                                    []
                           )
                    )
                ]
    }
