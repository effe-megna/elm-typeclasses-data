module Data.These exposing (..)

import Typeclasses.Semigroup as S
import Typeclasses.Show as Show


type These e a
    = Both ( e, a )
    | Left e
    | Right a


map : (a -> b) -> These e a -> These e b
map f t =
    bimap ( identity, f ) t


mapLeft : (e -> g) -> These e a -> These g a
mapLeft f t =
    bimap ( f, identity ) t


bimap : ( e -> g, a -> b ) -> These e a -> These g b
bimap ( g, f ) t =
    case t of
        Left e ->
            Left (g e)

        Right a ->
            Right (f a)

        Both ( e, a ) ->
            Both ( g e, f a )


andThen : (a -> These e b) -> These e a -> These e b
andThen f t =
    case t of
        Left e ->
            Left e

        Right a ->
            f a

        Both ( e, a ) ->
            case f a of
                Left _ ->
                    Left e

                Right aa ->
                    Right aa

                Both ( ee, aa ) ->
                    Both ( ee, aa )


swap : These e a -> These a e
swap t =
    case t of
        Left e ->
            Right e

        Right a ->
            Left a

        Both ( e, a ) ->
            Both ( a, e )


leftOnly : These e a -> Maybe e
leftOnly t =
    unwrapLeft Nothing Just <| t


rightOnly : These e a -> Maybe a
rightOnly t =
    unwrap Nothing Just <| t


unwrap : b -> (a -> b) -> These e a -> b
unwrap default f t =
    case t of
        Left _ ->
            default

        Right a ->
            f a

        Both ( _, a ) ->
            f a


unwrapLeft : g -> (e -> g) -> These e a -> g
unwrapLeft default f t =
    case t of
        Left e ->
            f e

        Right _ ->
            default

        Both ( e, _ ) ->
            f e


getSemigroup : S.Semigroup e -> S.Semigroup a -> S.Semigroup (These e a)
getSemigroup se sa =
    S.makeSemigroup
        (\a b ->
            case a of
                Left ea ->
                    case b of
                        Left eb ->
                            Left (se.concat ea eb)

                        Right ab ->
                            Right ab

                        Both ( eb, ab ) ->
                            Both ( se.concat ea eb, ab )

                Right aa ->
                    case b of
                        Left eb ->
                            Left eb

                        Right ab ->
                            Right (sa.concat aa ab)

                        Both ( eb, ab ) ->
                            Both ( eb, sa.concat aa ab )

                Both ( ea, aa ) ->
                    case b of
                        Left eb ->
                            Left eb

                        Right ab ->
                            Right ab

                        Both ( eb, ab ) ->
                            Both ( se.concat ea eb, sa.concat aa ab )
        )


getShow : Show.Show e -> Show.Show a -> Show.Show (These e a)
getShow showE showA =
    Show.makeShow
        (\t ->
            case t of
                Left e ->
                    "Left(" ++ showE.show e ++ ")"

                Right a ->
                    "Right(" ++ showA.show a ++ ")"

                Both ( e, a ) ->
                    "Both(" ++ showE.show e ++ "," ++ showA.show a ++ ")"
        )
