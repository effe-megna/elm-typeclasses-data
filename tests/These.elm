module These exposing (..)

import Data.These as THS
import Expect as Expect
import Test exposing (..)
import Typeclasses.Semigroup as S
import Typeclasses.Show as Show


suite : Test
suite =
    describe "These Suite"
        [ test "map" <|
            \_ ->
                THS.Right 5
                    |> THS.map (\n -> n + 5)
                    |> THS.map (\n -> n + 4)
                    |> Expect.equal (THS.Right 14)
        , test "mapLeft" <|
            \_ ->
                THS.Left "Elm"
                    |> THS.map (\s -> s ++ "-edgar")
                    |> THS.mapLeft (\s -> s ++ "-effe")
                    |> Expect.equal (THS.Left "Elm-effe")
        , test "bimap" <|
            \_ ->
                THS.Both ( "Left", True )
                    |> THS.bimap ( \e -> e ++ "-value", \a -> ( a, "Right-value" ) )
                    |> Expect.equal (THS.Both ( "Left-value", ( True, "Right-value" ) ))
        , test "andThen" <|
            \_ ->
                THS.Right ""
                    |> THS.andThen (\_ -> THS.Right "effe")
                    |> THS.map (\name -> name ++ "-emme")
                    |> Expect.equal (THS.Right "effe-emme")
        , test "unwrap with a Right value" <|
            \_ ->
                THS.Right 42
                    |> THS.unwrap False (\a -> a == 42)
                    |> Expect.equal True
        , test "right only" <|
            \_ ->
                THS.Both("nothing", 42)
                    |> THS.rightOnly
                    |> Expect.equal (Just 42)
        , test "left only" <|
            \_ ->
                THS.Left "22"
                    |> THS.leftOnly
                    |> Expect.equal (Just "22")
        , test "swap with a Both value" <|
            \_ ->
                THS.Both ( "hello", 23 )
                    |> THS.swap
                    |> Expect.equal (THS.Both ( 23, "hello" ))
        , test "show" <|
            \_ ->
                let
                    myShow =
                        THS.getShow Show.stringShow Show.stringShow
                in
                THS.Both ( "Oh I'm a left-value", "AA" )
                    |> myShow.show
                    |> Expect.equal "Both(Oh I'm a left-value,AA)"
        , test "semigroup" <|
            \_ ->
                let
                    semigroup =
                        THS.getSemigroup S.stringSemigroup S.andSemigroup
                in
                semigroup.concat (THS.Both ( "2", True )) (THS.Both ( "11", True ))
                    |> Expect.equal (THS.Both ( "211", True ))
        ]
