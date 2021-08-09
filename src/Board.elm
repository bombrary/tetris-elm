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
            if List.any (Vec.equal (indexToVec i)) positions then
                cell
            else
                c
        )
        board


overlapped : List (Vec Int) -> Board -> Bool
overlapped positions board =
    (overlappedWall positions) || (overlappedBoard positions board)



overlappedWall : List (Vec Int) -> Bool
overlappedWall positions =
    List.any (not << containedBoard) positions


containedBoard : Vec Int -> Bool
containedBoard { x, y } =
    0 <= x && x < width && y < height


overlappedBoard : List (Vec Int) -> Board -> Bool
overlappedBoard positions board =
    List.any (overlappedOne board) positions


overlappedOne : Board -> Vec Int -> Bool
overlappedOne board v =
    List.any identity <|
        List.indexedMap (\i c -> Vec.equal (indexToVec i) v && c /= Empty) board


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
