module Mino exposing (..)

import Color exposing (Color(..))
import Random exposing (Generator)
import Random.List as Random
import Util
import Vec exposing (Vec)


type Mino
    = I
    | L
    | J
    | Z
    | S
    | O
    | T


type alias Info =
    { rotMax : Int
    , color : Color
    , p1 : Vec Int
    , p2 : Vec Int
    , p3 : Vec Int
    }


info : Mino -> Info
info mino =
    case mino of
        I ->
            Info 4 LightBlue (Vec -2 0) (Vec -1 0) (Vec 1 0)

        L ->
            Info 4 Orange (Vec -1 0) (Vec 1 0) (Vec 1 1)

        J ->
            Info 4 Blue (Vec -1 0) (Vec 1 0) (Vec 1 -1)

        Z ->
            Info 4 Red (Vec 1 0) (Vec 0 1) (Vec -1 1)

        S ->
            Info 4 Green (Vec -1 0) (Vec 0 1) (Vec 1 1)

        O ->
            Info 1 Yellow (Vec 0 1) (Vec 1 0) (Vec 1 1)

        T ->
            Info 4 Purple (Vec 0 -1) (Vec -1 0) (Vec 1 0)


calcRot : Int -> Info -> Int
calcRot r { rotMax } =
    modBy rotMax r


positions : Info -> List (Vec Int)
positions { p1, p2, p3 } =
    [ p1, p2, p3, Vec 0 0 ]


random : Generator Mino
random =
    Random.uniform I [ L, J, Z, S, O, T ]


randoms : Generator (List Mino)
randoms =
    Random.shuffle [ I, L, J, Z, S, O, T ]


type alias ViewBox =
    { x : Int
    , y : Int
    , width : Int
    , height : Int
    }


viewBox : Mino -> ViewBox
viewBox mino =
    let
        ps =
            positions (info mino)

        x0 =
            Vec.minimumX ps

        y0 =
            Vec.minimumY ps

        x1 =
            Vec.maximumX ps

        y1 =
            Vec.maximumY ps

        width =
            Maybe.map2 (\a b -> b - a + 1) x0 x1

        height =
            Maybe.map2 (\a b -> b - a + 1) y0 y1
    in
    Maybe.withDefault
        (ViewBox 0 0 0 0)
        (Maybe.map4 ViewBox x0 y0 width height)


rotate : Int -> Mino -> List (Vec Int)
rotate r mino =
    let
        ({ rotMax } as mInfo) =
            info mino

        ps =
            positions mInfo
    in
    case mino of
        I ->
            case modBy rotMax r of
                0 ->
                    ps

                1 ->
                    List.map Vec.rotate90 ps

                2 ->
                    List.map (Vec.add (Vec 0 1)) ps

                _ ->
                    List.map (Vec.add (Vec -1 0) << Vec.rotate90) ps

        _ ->
            List.map (Util.applyN (modBy rotMax r) Vec.rotate90) ps


type Direction
    = RotZero
    | RotRight
    | RotLeft
    | RotTwo


getDirection : Int -> Mino -> Direction
getDirection r mino =
    case mino of
        O ->
            RotZero

        _ ->
            let
                { rotMax } =
                    info mino
            in
            case modBy rotMax r of
                0 ->
                    RotZero

                1 ->
                    RotRight

                2 ->
                    RotTwo

                _ ->
                    RotLeft


getKickList : Direction -> Direction -> Mino -> List (Vec Int)
getKickList o1 o2 mino =
    case mino of
        I ->
            getKickListI o1 o2

        O ->
            [ Vec 0 0 ]

        _ ->
            getKickListJLSTZ o1 o2


getKickListJLSTZ : Direction -> Direction -> List (Vec Int)
getKickListJLSTZ o1 o2 =
    case (o1, o2) of
        (RotZero, RotRight) ->
             [Vec 0 0, Vec -1 0, Vec -1 -1, Vec 0 2, Vec -1 2]

        (RotRight, RotZero) ->
             [Vec 0 0, Vec 1 0, Vec 1 1, Vec 0 -2, Vec 1 -2]

        (RotRight, RotTwo) ->
             [Vec 0 0, Vec 1 0, Vec 1 1, Vec 0 -2, Vec 1 -2]

        (RotTwo, RotRight) ->
             [Vec 0 0, Vec -1 0, Vec -1 -1, Vec 0 2, Vec -1 2]

        (RotTwo, RotLeft) ->
             [Vec 0 0, Vec 1 0, Vec 1 -1, Vec 0 2, Vec 1 2]

        (RotLeft, RotTwo) ->
             [Vec 0 0, Vec -1 0, Vec -1 1, Vec 0 -2, Vec -1 -2]

        (RotLeft, RotZero) ->
             [Vec 0 0, Vec -1 0, Vec -1 1, Vec 0 -2, Vec -1 -2]

        (RotZero, RotLeft) ->
             [Vec 0 0, Vec 1 0, Vec 1 -1, Vec 0 2, Vec 1 2]

        _ ->
            [Vec 0 0]


getKickListI : Direction -> Direction -> List (Vec Int)
getKickListI o1 o2 =
    case (o1, o2) of
        (RotZero, RotRight) ->
             [Vec 0 0, Vec -2 0, Vec 1 0, Vec -2 1, Vec 1 -2]

        (RotRight, RotZero) ->
             [Vec 0 0, Vec 2 0, Vec -1 0, Vec 2 -1, Vec -1 2]

        (RotRight, RotTwo) ->
             [Vec 0 0, Vec -1 0, Vec 2 0, Vec -1 -2, Vec 2 1]

        (RotTwo, RotRight) ->
             [Vec 0 0, Vec 1 0, Vec -2 0, Vec 1 2, Vec -2 -1]

        (RotTwo, RotLeft) ->
             [Vec 0 0, Vec 2 0, Vec -1 0, Vec 2 -1, Vec -1 2]

        (RotLeft, RotTwo) ->
             [Vec 0 0, Vec -2 0, Vec 1 0, Vec -2 1, Vec 1 -2]

        (RotLeft, RotZero) ->
             [Vec 0 0, Vec 1 0, Vec -2 0, Vec 1 2, Vec -2 -1]

        (RotZero, RotLeft) ->
             [Vec 0 0, Vec -1 0, Vec 2 0, Vec -1 -2, Vec 2 1]

        _ ->
            [Vec 0 0]
