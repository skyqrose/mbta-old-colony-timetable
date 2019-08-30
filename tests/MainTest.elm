module MainTest exposing (suite)

import Expect
import Test exposing (Test, describe, test)


suite : Test
suite =
    test "have at least one test so npm test passes" <|
        \_ ->
            Expect.equal
                True
                True
