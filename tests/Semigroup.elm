module Semigroup exposing (..)

import Expect as Expect
import Test exposing (..)
import Typeclasses.Semigroup as S


suite : Test
suite =
    describe "Semigroup Suite"
        [ test "String Semigroup" <|
            \_ ->
                S.appendable.concat "elm" "elm"
                    |> Expect.equal "elmelm"
        , test "Int Semigroup" <|
            \_ ->
                S.sumSemigroup.concat 42 42
                    |> Expect.equal 84
        , test "Make Custom Semigroup" <|
            \_ ->
                let
                    userSemigroup =
                        S.makeSemigroup
                            (\a b ->
                                { firstname = S.appendable.concat a.firstname b.firstname }
                            )
                in
                userSemigroup.concat { firstname = "effe" } { firstname = "effe" }
                    |> Expect.equal { firstname = "effeeffe" }
        ]
