module Typeclasses.Functor exposing (..)


type alias Functor a b fa fb =
    { map : (a -> b) -> fa -> fb
    }

mapList : Functor a b (List a) (List b)
mapList = 
  { map = List.map }