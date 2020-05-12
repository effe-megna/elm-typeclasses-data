module Typeclasses.Show exposing (..)


type alias Show a =
    { show : a -> String
    }


makeShow : (a -> String) -> Show a
makeShow show =
    { show = show }


stringShow : Show String
stringShow =
    makeShow identity

intShow : Show Int
intShow = 
    makeShow String.fromInt

charShow : Show Char
charShow =
    makeShow String.fromChar

boolShow : Show Float
boolShow = 
    makeShow String.fromFloat