module Util exposing (..)


applyN : Int -> (a -> a) -> a -> a
applyN n f x0 =
    let
        go cnt x =
            if cnt <= 0 then
                x

            else
                go (cnt - 1) (f x)
    in
    go n x0


