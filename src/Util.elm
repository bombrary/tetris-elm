module Util exposing (..)


applyN : Int -> (a -> a) -> a -> a
applyN n f = List.foldr (<<) identity (List.repeat n f)
