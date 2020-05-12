module Monoid exposing (..)

import Expect as Expect
import Test exposing (..)
import Typeclasses.Monoid as M
import Typeclasses.Semigroup as S


suite : Test
suite =
    describe "Monoid Suite"
        [ test "String Monoid" <|
            \_ ->
                M.monoidString.concat "elm" "elm"
                    |> Expect.equal "elmelm"
        , test "Int Monoid" <|
            \_ ->
                M.monoidSum.concat 42 42
                    |> Expect.equal 84
        , test "Make custom Monoid" <|
            \_ ->
                let
                    semigroup =
                        S.makeSemigroup (\a b -> { firstname = M.monoidString.concat a.firstname b.firstname })

                    userMonoid =
                        M.makeMonoid ( semigroup, { firstname = "" } )
                in
                userMonoid.concat { firstname = "effe" } { firstname = "effe" }
                    |> Expect.equal { firstname = "effeeffe" }
        , test "Monoid fold on custom Type alias" <|
            \_ ->
                let
                    semigroup =
                        S.makeSemigroup (\a b -> { firstname = M.monoidString.concat a.firstname b.firstname })

                    userMonoid =
                        M.makeMonoid ( semigroup, { firstname = "" } )

                    users =
                        [ { firstname = "effe" }, { firstname = "edgar" }, { firstname = "bigG" } ]
                in
                M.fold userMonoid users
                    |> Expect.equal { firstname = "effeedgarbigG" }
        , test "Monoid fold on (Int -> Bool) using getFunctionMonoid" <|
            \_ ->
                let
                    isEven n =
                        modBy 2 n == 0

                    isGt10 n =
                        n > 10

                    isLt20 n =
                        n < 20

                    predicates =
                        [ isEven, isGt10, isLt20 ]
                in
                M.fold predicateFunctionMonoid predicates 12
                    |> Expect.equal True
        ]


predicateSemigroup : S.Semigroup Bool
predicateSemigroup =
    S.makeSemigroup S.andSemigroup.concat


predicateMonoid : M.Monoid Bool
predicateMonoid =
    M.makeMonoid ( predicateSemigroup, True )


predicateFunctionMonoid : M.Monoid (Int -> Bool)
predicateFunctionMonoid =
    M.getFunctionMonoid predicateMonoid
