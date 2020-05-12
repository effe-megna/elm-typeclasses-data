module Functor exposing (..)

import Expect as Expect
import Test exposing (..)
import Typeclasses.Functor as F


suite : Test
suite =
    describe "Functor Suite"
        [ test "List Functor" <|
            \_ ->
                F.mapList.map (\a -> a + 1) [1, 2, 3]
                  |> Expect.equal [2, 3, 4]
        ]
