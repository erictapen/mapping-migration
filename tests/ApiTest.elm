module ApiTest exposing (suite)

import Api exposing (asylumDecisionsDecoder)
import ApiTestStrings exposing (..)
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Json.Decode exposing (decodeString)
import Test exposing (..)


suite : Test
suite =
    test "asylumDecisionsDecoder" <|
        \_ ->
            Expect.ok <|
                decodeString asylumDecisionsDecoder asylumDecisionsAlg
