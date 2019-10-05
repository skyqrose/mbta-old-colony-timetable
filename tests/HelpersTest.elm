module HelpersTest exposing (suite)

import Expect
import Helpers
import Test exposing (..)


suite : Test
suite =
    describe "Helpers"
        [ describe "uniq"
            [ test "removes duplicates" <|
                \_ ->
                    Expect.equal
                        (Helpers.uniq [ 1, 3, 2, 1, 2 ])
                        [ 1, 3, 2 ]
            ]
        ]
