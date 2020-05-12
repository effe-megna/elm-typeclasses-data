module Typeclasses.Monoid exposing (..)

import Typeclasses.Semigroup as S

type alias Monoid a =
    { concat : a -> a -> a
    , empty : a
    }


makeMonoid : ( S.Semigroup a, a ) -> Monoid a
makeMonoid ( semigroup, empty ) =
    { concat = semigroup.concat, empty = empty }

-- PRIMITIVES


monoidAll : Monoid Bool
monoidAll =
    makeMonoid ( S.andSemigroup, True )


monoidAny : Monoid Bool
monoidAny =
    makeMonoid ( S.orSemigroup, False )


monoidSum : Monoid Int
monoidSum =
    makeMonoid ( S.sumSemigroup, 0 )


monoidProduct : Monoid Int
monoidProduct =
    makeMonoid ( S.productSemigroup, 0 )


monoidString : Monoid String
monoidString =
    makeMonoid ( S.stringSemigroup, "" )


getFunctionMonoid : Monoid m -> Monoid (a -> m)
getFunctionMonoid m =
    let
        functionSemigroup =
            S.getFunctionSemigroup { concat = m.concat }
    in
    makeMonoid ( functionSemigroup, \_ -> m.empty )


fold : Monoid a -> List a -> a
fold monoid list =
    let
        foldM =
            S.foldSemigroup { concat = monoid.concat }
    in
    foldM ( monoid.empty, list )
