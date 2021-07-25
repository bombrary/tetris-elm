module MinoQueue exposing (..)


type alias MinoQueue a =
    { now : List a
    , next : List a
    }


empty : MinoQueue a
empty =
    { now = []
    , next = []
    }


takeOne : MinoQueue a -> Maybe ( a, MinoQueue a )
takeOne ({ now } as q) =
    case now of
        [] ->
            Nothing

        x :: xs ->
            Just ( x, { q | now = xs } )


insertToNext : List a -> MinoQueue a -> MinoQueue a
insertToNext list q =
    { q | next = list }


swap : MinoQueue a -> MinoQueue a
swap ({ now, next } as q) =
    { q | now = next, next = now }


take : Int -> MinoQueue a -> List a
take n { now, next } =
    List.take n (now ++ next)
    
