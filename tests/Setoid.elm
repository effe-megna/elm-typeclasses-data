module Setoid exposing (..)

import Expect as Expect
import Test exposing (..)
import Typeclasses.Setoid as S


suite : Test
suite =
    describe "Setoid Suite"
        [ test "String Setoid" <|
            \_ ->
                S.stringSetoid.equals "elm" "elm"
                    |> Expect.equal True
        , test "Int Setoid" <|
            \_ ->
                S.intSetoid.equals 42 42
                    |> Expect.equal True
        , test "Make Custom Setoid" <|
            \_ ->
                let
                    userSetoid =
                        S.makeSetoid (\a b -> a.firstname == b.firstname)
                in
                userSetoid.equals { firstname = "effe" } { firstname = "effe" }
                    |> Expect.equal True
        ]
