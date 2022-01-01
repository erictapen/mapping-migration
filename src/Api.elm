module Api exposing (Country, fetchCountries, fetchAsylumDecisions, handleGotCountries, unknownCountry, AvailableCOAs, asylumDecisionsDecoder)

import Dict exposing (Dict, insert, update)
import Http
import Json.Decode as JD exposing (int, maybe, string, succeed)
import Json.Decode.Pipeline exposing (optional, required)
import List exposing (foldr, map)
import Maybe exposing (withDefault)



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

    -- UNHCR three letter country code notation
    , code : CountryCode
    }


type alias CountryCode =
    String



-- Like any good API, the UNHCR API is ambigous about their types...
-- E.g. they encode a 0 as "0"...
-- TODO: Make sure only 0 are encoded as strings, as we currently assume 0 when
-- we discover a string!


ambigousNumber : JD.Decoder Int
ambigousNumber =
    JD.oneOf [ int, succeed 0 ]


fetchCountries msgConstructor =
    Http.request
        { method = "GET"
        , headers = headers
        , url = "https://api.unhcr.org/population/v1/countries/"
        , body = Http.emptyBody
        , expect = Http.expectJson msgConstructor countriesDecoder
        , timeout = Nothing
        , tracker = Nothing
        }


countriesDecoder : JD.Decoder (List Country)
countriesDecoder =
    JD.field "items" <|
        JD.list <|
            JD.map2 Country
                (JD.field "name" JD.string)
                (JD.field "code" JD.string)


country name code =
    { name = name, code = code }


unknownCountry =
    { name = "Unknown", code = "UKN" }


handleGotCountries model countryNamesResult =
    ( { model
        | availableCountries =
            Result.toMaybe countryNamesResult
      }
    , Cmd.none
    )


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
    , decisionsTotal : Int
    }


type ProcedureType
    = Government
    | Joint
    | UNHCR


type ApplicationType
    = New
    | Repeat
    | Appeal


fetchAsylumDecisions msgConstructor coo =
    Http.request
        { method = "GET"
        , headers = headers
        , url = "https://api.unhcr.org/population/v1/asylum-decisions/?coa_all=true&coo=" ++ coo.code
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
                |> required "dec_total" int
            )


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


availableCOAs : List AsylumDecisionsJson -> AvailableCOAs
availableCOAs =
    foldr buildAvailableCOAs Dict.empty


buildAvailableCOAs : AsylumDecisionsJson -> AvailableCOAs -> AvailableCOAs
buildAvailableCOAs obj old =
    let
        coa o =
            { procedureType = o.procedureType
            , applicationType = o.applicationType
            , decisionsTotal = o.decisionsTotal
            }

        updateCOA value =
            Just <| insert obj.year (coa obj) <| withDefault Dict.empty value
    in
    update obj.coa_code updateCOA old
