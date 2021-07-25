module Position exposing (..)

import Vec exposing (..)

type alias Position a =
    { pos : Vec Int
    , val : a
    }
