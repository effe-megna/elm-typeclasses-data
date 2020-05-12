module Typeclasses.Magma exposing (..)


type alias Magma a =
    { concat : a -> a -> a
    }
