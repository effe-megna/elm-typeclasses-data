module Apply exposing (..)

import Expect as Expect
import Test exposing (..)
import Typeclasses.Apply as A


suite : Test
suite =
    describe "Apply Suite"
        [ test "List Apply" <|
            \_ ->
                True
                  |> Expect.equal True
        ]
