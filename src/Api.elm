module Api exposing
    ( AsylumDecisions
    , AvailableCOAs
    , COA
    , Country
    , CountryCode
    , PersonsOrCases(..)
    , Year
    , asylumDecisionsDecoder
    , asylumDecisionsPath
    , compareCountryCode
    , compareCountryCode2
    , fetchAsylumDecisions
    , fetchCountries
    , unknownCountry
    , unknownCountryCode
    )

import Data
import Dict exposing (Dict, insert, update)
import Http
import Json.Decode as JD exposing (int, maybe, string, succeed)
import Json.Decode.Pipeline exposing (optional, required)
import List exposing (foldr, map)
import Maybe exposing (withDefault)



-- baseUrl = "https://api.unhcr.org"


baseUrl =
    "https://mappingmigration.erictapen.name/unhcr-api"



-- Some words on UNHCR terminology:
-- - COO: Country of origin
-- - COA: Country of asylum


userAgentHeader =
    Http.header "User-Agent" "Please contact me at unhcr@erictapen.name if there is a problem with my API usage."


acceptHeader =
    Http.header "Accept" "application/json"


headers =
    -- [ userAgentHeader, acceptHeader ]
    -- for some reasen this produces a CORS error?
    [ acceptHeader ]


type alias Country =
    { name : String
    , iso : String
    }


{-| UNHCR three letter country code notation
Not to confuse with the ISO codes!
-}
type alias CountryCode =
    String


compareCountryCode : CountryCode -> CountryCode -> Order
compareCountryCode =
    compare


{-| Helper function that helps when the CountryCode is embedded in some other structure
-}
compareCountryCode2 : (a -> CountryCode) -> a -> a -> Order
compareCountryCode2 f c1 c2 =
    compareCountryCode (f c1) (f c2)


{-| Like any good API, the UNHCR API is ambigous about their types...
E.g. they encode a 0 as "0"...
TODO: Make sure only 0 are encoded as strings, as we currently assume 0 when
we discover a string!
-}
ambigousNumber : JD.Decoder Int
ambigousNumber =
    JD.oneOf [ int, succeed 0 ]


fetchCountries msgConstructor =
    Http.request
        { method = "GET"
        , headers = headers
        , url = baseUrl ++ "/population/v1/countries/"
        , body = Http.emptyBody
        , expect = Http.expectJson msgConstructor countriesDecoder
        , timeout = Nothing
        , tracker = Nothing
        }


triple : a -> b -> c -> ( a, b, c )
triple a b c =
    ( a, b, c )


countriesDecoder : JD.Decoder (Dict CountryCode Country)
countriesDecoder =
    JD.andThen (countriesDict >> JD.succeed) <|
        JD.field "items" <|
            JD.list <|
                JD.map3 triple
                    (JD.field "code" JD.string)
                    (JD.field "name" JD.string)
                    (JD.field "iso" (JD.nullable JD.string))


countriesDict : List ( CountryCode, String, Maybe String ) -> Dict CountryCode Country
countriesDict unsortedPairs =
    let
        unsortedCountries =
            List.filterMap
                (\( cc, str, maybeIso ) ->
                    case maybeIso of
                        Nothing ->
                            Nothing

                        Just iso ->
                            Just ( cc, Country str iso )
                )
                unsortedPairs
    in
    Dict.fromList <|
        List.sortWith (compareCountryCode2 Tuple.first) unsortedCountries


country name code =
    { name = name, code = code }


unknownCountryCode =
    "UKN"


unknownCountry =
    { name = "Unknown"
    , iso = "UKN"
    }


{-| Intermediate representation of the JSON output to make decoding easier to comprehend.
-}
type alias AsylumDecisionsJson =
    { year : Year
    , coo_code : String
    , coo_name : String
    , coa_code : String
    , coa_name : String
    , decisionsRecognized : Maybe Int
    , decisionsOther : Maybe Int
    , decisionsRejected : Maybe Int
    , decisionsClosed : Maybe Int
    , decisionsTotal : Int
    , personsOrCases : PersonsOrCases
    }


asylumDecisionsDecoder : JD.Decoder AvailableCOAs
asylumDecisionsDecoder =
    JD.andThen (availableCOAs >> succeed) <|
        JD.field "items" <|
            JD.list <|
                (JD.succeed AsylumDecisionsJson
                    |> required "year" int
                    |> required "coo" string
                    |> required "coo_name" string
                    |> required "coa" string
                    |> required "coa_name" string
                    |> optional "dec_recognized" (maybe ambigousNumber) Nothing
                    |> optional "dec_other" (maybe ambigousNumber) Nothing
                    |> optional "dec_rejected" (maybe ambigousNumber) Nothing
                    |> optional "dec_closed" (maybe ambigousNumber) Nothing
                    |> required "dec_total" ambigousNumber
                    |> required "dec_pc" personsOrCases
                )


{-| All the available data for a given COO
-}
type alias AvailableCOAs =
    Dict CountryCode COA


{-| Country of asylum
-}
type alias COA =
    Dict Year AsylumDecisions


type alias Year =
    Int


type PersonsOrCases
    = Persons
    | Cases


personsOrCases : JD.Decoder PersonsOrCases
personsOrCases =
    JD.andThen
        (\str ->
            case str of
                "P" ->
                    JD.succeed Persons

                "C" ->
                    JD.succeed Cases

                unknownType ->
                    JD.fail <| "Unknown dec_pc " ++ unknownType
        )
        JD.string


type alias AsylumDecisions =
    { decisionsRecognized : Maybe Int
    , decisionsOther : Maybe Int
    , decisionsRejected : Maybe Int
    , decisionsClosed : Maybe Int
    , decisionsTotal : Int
    , personsOrCases : PersonsOrCases
    }


mergeAsylumDecisions : AsylumDecisions -> AsylumDecisions -> AsylumDecisions
mergeAsylumDecisions origin addition =
    { decisionsRecognized = maybeAdd origin.decisionsRecognized addition.decisionsRecognized
    , decisionsOther = maybeAdd origin.decisionsOther addition.decisionsOther
    , decisionsRejected = maybeAdd origin.decisionsRejected addition.decisionsRejected
    , decisionsClosed = maybeAdd origin.decisionsClosed addition.decisionsClosed
    , decisionsTotal = origin.decisionsTotal + addition.decisionsTotal
    , personsOrCases = origin.personsOrCases
    }


maybeAdd : Maybe Int -> Maybe Int -> Maybe Int
maybeAdd m1 m2 =
    case m1 of
        Nothing ->
            m2

        Just i1 ->
            case m2 of
                Nothing ->
                    Just i1

                Just i2 ->
                    Just <| i1 + i2


{-| This should be high enough to always get all the entries at once.
-}
paginationLimit =
    "10000"


asylumDecisionsPath : CountryCode -> String
asylumDecisionsPath coo =
    baseUrl
        ++ "/population/v1/asylum-decisions/?coa_all=true&limit="
        ++ paginationLimit
        ++ "&coo="
        ++ coo


fetchAsylumDecisions msgConstructor coo =
    Http.request
        { method = "GET"
        , headers = headers
        , url = asylumDecisionsPath coo
        , body = Http.emptyBody
        , expect = Http.expectJson msgConstructor asylumDecisionsDecoder
        , timeout = Nothing
        , tracker = Nothing
        }


availableCOAs : List AsylumDecisionsJson -> AvailableCOAs
availableCOAs =
    foldr buildAvailableCOAs Dict.empty


buildAvailableCOAs : AsylumDecisionsJson -> AvailableCOAs -> AvailableCOAs
buildAvailableCOAs obj old =
    let
        coa =
            { decisionsRecognized = obj.decisionsRecognized
            , decisionsOther = obj.decisionsOther
            , decisionsRejected = obj.decisionsRejected
            , decisionsClosed = obj.decisionsClosed
            , decisionsTotal = obj.decisionsTotal
            , personsOrCases = obj.personsOrCases
            }

        updateYear value =
            case value of
                Nothing ->
                    Just coa

                Just origin ->
                    Just <| mergeAsylumDecisions origin coa

        updateCOA value =
            Just <| update obj.year updateYear <| withDefault Dict.empty value
    in
    update obj.coa_code updateCOA old
