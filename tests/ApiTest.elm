module ApiTest exposing (suite)

import Api exposing (asylumDecisionsDecoder)
import ApiTestStrings exposing (..)
import Dict
import Expect
import Json.Decode as JD exposing (decodeString)
import Test exposing (..)


suite : Test
suite =
    test "asylumDecisionsDecoder" <|
        \_ ->
            let
                aDResult =
                    decodeString asylumDecisionsDecoder asylumDecisionsALG

                count aD =
                    List.foldr (\coaDict -> (+) <| Dict.size coaDict) 0 <| Dict.values aD
            in
            Expect.all
                [ Expect.ok

                -- best guess is 74/100 currently
                , Expect.equal (Ok 74) << Result.map count
                ]
                aDResult
