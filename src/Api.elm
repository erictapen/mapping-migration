module Api exposing (Country, fetchCountries, handleGotCountries, unknownCountry)

import Http
import Json.Decode as JD


type alias Country =
    { name : String

    -- UNHCR three letter country code notation
    , code : String
    }


fetchCountries msgConstructor =
    Http.request
        { method = "GET"
        , headers = [ Http.header "Accept" "application/json" ]
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


handleGotCountries model countryNames =
    ( { model
        | availableCountries =
            Result.toMaybe countryNames
      }
    , Cmd.none
    )
