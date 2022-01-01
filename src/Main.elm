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
    ( { coo = unknownCountry
      , coa = unknownCountry
      , countries = NotLoaded
      , availableCOAs = NotLoaded
      , stickFigures =
            [ { position = ( 0, 0 )
              , start = ( 0, 0 )
              , destination = ( 100, 100 )
              }
            ]
      }
    , fetchCountries GotCountries
    )


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


type alias Model =
    { coo : Country
    , coa : Country
    , stickFigures : List StickFigure
    , countries : Loadable (Dict CountryCode Country)
    , availableCOAs : Loadable AvailableCOAs
    }


type Msg
    = Tick Time.Posix
    | ChangeCoo CountryCode
    | ChangeCoa CountryCode
    | GotCountries (Result Http.Error (Dict CountryCode Country))
    | GotAsylumDecisions (Result Http.Error AvailableCOAs)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick _ ->
            ( { model | stickFigures = map moveStickFigure model.stickFigures }, Cmd.none )

        ChangeCoo countryCode ->
            let
                coo =
                    withDefault unknownCountry <| Dict.get countryCode <| withDefault Dict.empty <| unwrapLoadable model.countries
            in
            ( { model
                | coo = coo
                , availableCOAs = Loading
              }
            , fetchAsylumDecisions GotAsylumDecisions model.coo
            )

        GotCountries countriesResult ->
            ( { model
                | countries = Loaded <| Result.toMaybe countriesResult
                , coo =
                    withDefault unknownCountry <|
                        Maybe.map Tuple.second <|
                            head <|
                                List.sortBy Tuple.first <|
                                    Dict.toList <|
                                        Result.withDefault Dict.empty countriesResult
              }
            , fetchAsylumDecisions GotAsylumDecisions model.coo
            )

        GotAsylumDecisions asylumDecisionsResult ->
            ( { model
                | availableCOAs = Loaded <| Result.toMaybe asylumDecisionsResult
                , coa =
                    withDefault unknownCountry <|
                        Maybe.andThen
                            (\cc ->
                                Dict.get cc <|
                                    withDefault Dict.empty <|
                                        unwrapLoadable model.countries
                            )
                        <|
                            Maybe.map Tuple.first <|
                                head <|
                                    List.sortBy Tuple.first <|
                                        Dict.toList <|
                                            Result.withDefault Dict.empty asylumDecisionsResult
              }
            , Cmd.none
            )

        ChangeCoa countryCode ->
            let
                coa =
                    withDefault unknownCountry <| Dict.get countryCode <| withDefault Dict.empty <| unwrapLoadable model.countries
            in
            ( { model | coa = coa }, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


countryOption { name, code } =
    option [ value code ] [ text (name ++ " (" ++ code ++ ")") ]


cooSelect loadableCountries =
    case loadableCountries of
        NotLoaded ->
            text ""

        Loading ->
            text "Loading countries..."

        Loaded countriesMaybe ->
            case countriesMaybe of
                Nothing ->
                    text "An error occured while fetching asylumDecisions!"

                Just countries ->
                    fieldset []
                        [ legend [] [ text "country of origin" ]
                        , select [ onInput ChangeCoo ] <| map countryOption <| List.sortBy .name <| Dict.values countries
                        ]


coaSelect countries loadableCOAs =
    case loadableCOAs of
        NotLoaded ->
            text ""

        Loading ->
            text "loading..."

        Loaded coasMaybe ->
            case coasMaybe of
                Nothing ->
                    text "An error occured while fetching asylumDecisions!"

                Just coas ->
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
                                        (\c ->
                                            withDefault unknownCountry <|
                                                Dict.get c <|
                                                    withDefault Dict.empty <|
                                                        unwrapLoadable countries
                                        )
                                    <|
                                        Dict.keys coas
                            ]


coaVis : Maybe COA -> Html Msg
coaVis maybeCoa =
    case maybeCoa of
        Nothing ->
            text ""

        Just coa ->
            div [] <| map coaYearVis <| List.sortBy Tuple.first <| Dict.toList coa


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
        [ div []
            [ cooSelect model.countries
            , coaSelect model.countries model.availableCOAs
            , coaVis <| Dict.get model.coa.code <| withDefault Dict.empty <| unwrapLoadable model.availableCOAs
            , br [] []
            ]
        ]
    }
