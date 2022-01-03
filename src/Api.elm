module Api exposing
    ( ApplicationType(..)
    , AsylumDecisions
    , AvailableCOAs
    , COA
    , Country
    , CountryCode
    , ProcedureType(..)
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
    }


{-| UNHCR three letter country code notation
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


countriesDecoder : JD.Decoder (Dict CountryCode Country)
countriesDecoder =
    JD.andThen (countriesDict >> JD.succeed) <|
        JD.field "items" <|
            JD.list <|
                JD.map2 Tuple.pair
                    (JD.field "code" JD.string)
                    (JD.field "name" JD.string)


countriesDict : List ( CountryCode, String ) -> Dict CountryCode Country
countriesDict unsortedPairs =
    let
        unsortedCountries =
            map (\( cc, str ) -> ( cc, Country str )) unsortedPairs
    in
    Dict.fromList <|
        List.sortWith (compareCountryCode2 Tuple.first) unsortedCountries


country name code =
    { name = name, code = code }


unknownCountryCode =
    "UKN"


unknownCountry =
    { name = "Unknown" }


{-| Intermediate representation of the JSON output to make decoding easier to comprehend.
-}
type alias AsylumDecisionsJson =
    { year : Year
    , coo_code : String
    , coo_name : String
    , coa_code : String
    , coa_name : String
    , procedureType : ProcedureType
    , applicationType : Maybe ApplicationType
    , decisionsLevel : Maybe DecisionsLevel
    , decisionsRecognized : Maybe Int
    , decisionsOther : Maybe Int
    , decisionsRejected : Maybe Int
    , decisionsClosed : Maybe Int
    , decisionsTotal : Int
    }


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


type alias AsylumDecisions =
    { procedureType : ProcedureType
    , applicationType : Maybe ApplicationType
    , decisionsLevel : Maybe DecisionsLevel
    , decisionsRecognized : Maybe Int
    , decisionsOther : Maybe Int
    , decisionsRejected : Maybe Int
    , decisionsClosed : Maybe Int
    , decisionsTotal : Int
    }


asylumDecisionsPath : CountryCode -> String
asylumDecisionsPath coo =
    baseUrl ++ "/population/v1/asylum-decisions/?coa_all=true&coo=" ++ coo


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
                    |> required "procedure_type" procedureType
                    |> optional "app_type" applicationType Nothing
                    |> optional "dec_level" decisionsLevel Nothing
                    |> optional "dec_recognized" (maybe int) Nothing
                    |> optional "dec_other" (maybe int) Nothing
                    |> optional "dec_rejected" (maybe int) Nothing
                    |> optional "dec_closed" (maybe int) Nothing
                    |> required "dec_total" int
                )


type ProcedureType
    = Government
    | Joint
    | UNHCR


procedureType : JD.Decoder ProcedureType
procedureType =
    JD.andThen
        (\str ->
            case str of
                "G" ->
                    JD.succeed Government

                "J" ->
                    JD.succeed Joint

                "U" ->
                    JD.succeed UNHCR

                unknownType ->
                    JD.fail <| "Unknown procedure_type " ++ unknownType
        )
        JD.string


type ApplicationType
    = New
    | Repeat
    | Appeal


applicationType : JD.Decoder (Maybe ApplicationType)
applicationType =
    JD.andThen
        (\str ->
            case str of
                "N" ->
                    JD.succeed <| Just New

                "R" ->
                    JD.succeed <| Just Repeat

                "A" ->
                    JD.succeed <| Just Appeal

                unknownType ->
                    JD.fail <| "Unknown app_type " ++ unknownType
        )
        JD.string


type DecisionsLevel
    = NewApplications
    | FirstInstance
    | AdministrativeReview
    | RepeatedApplications
    | USCitizenShipAndImmigrationServices
    | USExecutiveOfficeOfImmigrationReview
    | JudicialReview
    | SubsidiaryProtection
    | FirstInstanceAndAppeal
    | TemporaryProtection
    | TemporaryAsylum
    | Backlog
    | TemporaryLeave
    | CantonalSwitzerland


decisionsLevelDict =
    Dict.fromList
        [ ( "NA", NewApplications )
        , ( "FI", FirstInstance )
        , ( "AR", AdministrativeReview )
        , ( "RA", RepeatedApplications )
        , ( "IN", USCitizenShipAndImmigrationServices )
        , ( "EO", USExecutiveOfficeOfImmigrationReview )
        , ( "JR", JudicialReview )
        , ( "SP", SubsidiaryProtection )
        , ( "FA", FirstInstanceAndAppeal )
        , ( "TP", TemporaryProtection )
        , ( "TA", TemporaryAsylum )
        , ( "BL", Backlog )
        , ( "TR", TemporaryLeave )
        , ( "CA", CantonalSwitzerland )
        ]


{-| TODO maybe fail here for unknown abbreviations?
-}
decisionsLevel : JD.Decoder (Maybe DecisionsLevel)
decisionsLevel =
    JD.map (\str -> Dict.get str decisionsLevelDict) JD.string


availableCOAs : List AsylumDecisionsJson -> AvailableCOAs
availableCOAs =
    foldr buildAvailableCOAs Dict.empty


buildAvailableCOAs : AsylumDecisionsJson -> AvailableCOAs -> AvailableCOAs
buildAvailableCOAs obj old =
    let
        coa o =
            { procedureType = o.procedureType
            , applicationType = o.applicationType
            , decisionsLevel = o.decisionsLevel
            , decisionsRecognized = o.decisionsRecognized
            , decisionsOther = o.decisionsOther
            , decisionsRejected = o.decisionsRejected
            , decisionsClosed = o.decisionsClosed
            , decisionsTotal = o.decisionsTotal
            }

        updateCOA value =
            Just <| insert obj.year (coa obj) <| withDefault Dict.empty value
    in
    update obj.coa_code updateCOA old
