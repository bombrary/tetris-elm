module Board exposing (..)

import Color exposing (..)
import Mino exposing (Mino)
import Position exposing (..)
import Vec exposing (Vec)


width : Int
width =
    10


height : Int
height =
    22


type Cell
    = Empty
    | Block Color


type alias Board =
    List Cell


indexToVec : Int -> Vec Int
indexToVec i =
    { x = modBy width i
    , y = i // width
    }


toPositions : Board -> List (Position Cell)
toPositions board =
    List.indexedMap
        (\i c ->
            { pos = indexToVec i
            , val = c
            }
        )
        board


putBlock : List (Vec Int) -> Cell -> Board -> Board
putBlock positions cell board =
    List.indexedMap
        (\i c ->
            let
                { x, y } = indexToVec i
            in
            if List.any (Vec.equal (indexToVec i)) positions then
                cell
            else
                c
        )
        board



maskMino : List (Position Cell) -> Board -> Board
maskMino positions board =
    List.indexedMap
        (\i c ->
            case getCellAtSamePosition (indexToVec i) positions of
                Just cell ->
                    cell

                Nothing ->
                    c
        )
        board


overlapped : List (Position Cell) -> Board -> Maybe (Vec Int)
overlapped positions board =
    maybeOr (overlappedWall positions) (overlappedBoard positions board)


maybeOr : Maybe a -> Maybe a -> Maybe a
maybeOr ma mb =
    case ma of
        Just _ ->
            ma

        Nothing ->
            mb


overlappedWall : List (Position Cell) -> Maybe (Vec Int)
overlappedWall positions =
    case positions of
        [] ->
            Nothing

        { pos } :: ps ->
            if not <| containedBoard pos then
                Just pos

            else
                overlappedWall ps


containedBoard : Vec Int -> Bool
containedBoard { x, y } =
    0 <= x && x < width && y < height


overlappedBoard : List (Position Cell) -> Board -> Maybe (Vec Int)
overlappedBoard positions board =
    case positions of
        [] ->
            Nothing

        p :: ps ->
            if overlappedOne p board then
                Just p.pos

            else
                overlappedBoard ps board


overlappedOne : Position Cell -> Board -> Bool
overlappedOne p board =
    List.any identity <|
        List.indexedMap (\i c -> Vec.equal (indexToVec i) p.pos && c /= Empty) board


getCellAtSamePosition : Vec Int -> List (Position Cell) -> Maybe Cell
getCellAtSamePosition v positions =
    List.filter (\{ pos } -> Vec.equal v pos) positions
        |> List.map (\{ val } -> val)
        |> List.head


eraseLines : Board -> ( Board, Int )
eraseLines board =
    let
        lines =
            filledLines board

        erasedLines =
            moveOver lines board
    in
    ( erasedLines, List.length lines )


filledLines : Board -> List Int
filledLines board =
    trueIndicies <|
        List.map
            (List.all (\c -> c /= Empty))
            (chunksOf width board)


trueIndicies : List Bool -> List Int
trueIndicies list =
    List.indexedMap Tuple.pair list
        |> List.filter (\( _, p ) -> p)
        |> List.map (\( i, _ ) -> i)


chunksOf : Int -> List a -> List (List a)
chunksOf n list =
    case List.take n list of
        [] ->
            []

        xs ->
            xs :: chunksOf n (List.drop n list)


moveOver : List Int -> Board -> Board
moveOver lines board =
    List.repeat ((List.length lines) * width) Empty
        ++ indexedFilter (\i _ -> not <| List.member (i // width) lines) board


indexedFilter : (Int -> a -> Bool) -> List a -> List a
indexedFilter pred list =
    List.indexedMap Tuple.pair list
        |> List.filter (\( i, x ) -> pred i x)
        |> List.map (\( _, x ) -> x)


init : Board
init =
    List.repeat (width * height) Empty


clip : Int -> Board -> Board
clip rows board =
    List.drop (width * rows) board
