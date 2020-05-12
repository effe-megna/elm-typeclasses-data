module Typeclasses.Setoid exposing (..)


type alias Setoid a =
    { equals : a -> a -> Bool
    }


makeSetoid : (a -> a -> Bool) -> Setoid a
makeSetoid equals =
    { equals = equals }


-- PRIMITIVES


boolSetoid : Setoid Bool
boolSetoid =
    makeSetoid (==)


intSetoid : Setoid Int
intSetoid =
    makeSetoid (==)


floatSetoid : Setoid Float
floatSetoid =
    makeSetoid (==)


charSetoid : Setoid Char
charSetoid =
    makeSetoid (==)


stringSetoid : Setoid String
stringSetoid =
    makeSetoid (==)