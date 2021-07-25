module Vec exposing (..)


type alias Vec a =
    { x : a
    , y : a
    }


mult : number -> Vec number -> Vec number
mult c { x, y } =
    { x = c * x
    , y = c * y
    }


toFloat : Vec Int -> Vec Float
toFloat { x, y } =
    { x = Basics.toFloat x
    , y = Basics.toFloat y
    }


fromTuple : ( number, number ) -> Vec number
fromTuple ( x, y ) =
    { x = x, y = y }


add : Vec number -> Vec number -> Vec number
add v1 v2 =
    { x = v1.x + v2.x
    , y = v1.y + v2.y
    }


equal : Vec number -> Vec number -> Bool
equal v1 v2 =
    v1.x == v2.x && v1.y == v2.y


rotate90 : Vec number -> Vec number
rotate90 { x, y } =
    { x = y 
    , y = -x
    }


maximumX : List (Vec comparable) -> Maybe comparable
maximumX = List.maximum << List.map (\{x} -> x)

maximumY : List (Vec comparable) -> Maybe comparable
maximumY = List.maximum << List.map (\{y} -> y)

minimumX : List (Vec comparable) -> Maybe comparable
minimumX = List.minimum << List.map (\{x} -> x)

minimumY : List (Vec comparable) -> Maybe comparable
minimumY = List.minimum << List.map (\{y} -> y)


flipY : Vec number -> Vec number
flipY v =
    { v | y = -v.y }
