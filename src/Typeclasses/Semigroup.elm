module Typeclasses.Semigroup exposing (..)

import Typeclasses.Magma exposing (Magma)


type alias Semigroup a =
    Magma a



makeSemigroup : (a -> a -> a) -> Semigroup a
makeSemigroup concat =
    { concat = concat }


appendable : Semigroup appendable
appendable =
    makeSemigroup (++)


andSemigroup : Semigroup Bool
andSemigroup =
    makeSemigroup (&&)


orSemigroup : Semigroup Bool
orSemigroup =
    makeSemigroup (||)


sumSemigroup : Semigroup Int
sumSemigroup =
    makeSemigroup (+)


productSemigroup : Semigroup Int
productSemigroup =
    makeSemigroup (*)


stringSemigroup : Semigroup String
stringSemigroup =
    makeSemigroup (++)


arraySemigroup : Semigroup (List a)
arraySemigroup =
    makeSemigroup List.append


firstSemigroup : Semigroup (List a)
firstSemigroup =
    makeSemigroup (\a _ -> a)


lastSemigroup : Semigroup (List a)
lastSemigroup =
    makeSemigroup (\_ b -> b)


getFunctionSemigroup : Semigroup s -> Semigroup (a -> s)
getFunctionSemigroup s =
    makeSemigroup (\f g a -> s.concat (f a) (g a))


foldSemigroup : Semigroup a -> ( a, List a ) -> a
foldSemigroup s ( a, listA ) =
    List.foldr s.concat a listA
