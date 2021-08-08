module Main exposing (..)

import Board exposing (Board, Cell(..))
import Browser
import Browser.Events
import Color exposing (Color(..))
import Css exposing (square)
import Html.Styled as H exposing (Html)
import Html.Styled.Attributes as HA
import Json.Decode as D
import Mino exposing (Mino)
import MinoQueue exposing (MinoQueue)
import Position exposing (Position)
import Random exposing (Seed)
import Svg.Styled as S exposing (Svg)
import Svg.Styled.Attributes as SA
import Time
import Util
import Vec exposing (Vec)



-- CONSTANT


cellIconLength : Float
cellIconLength =
    10.0


cellLength : Float
cellLength =
    20.0


boardWidth : Int
boardWidth =
    10


boardHeight : Int
boardHeight =
    20


tetrisSideWidth : Float
tetrisSideWidth =
    80.0


minoIconHeight : Float
minoIconHeight =
    20.0



-- MAIN


main =
    Browser.element
        { init = init
        , view = view >> H.toUnstyled
        , update = update
        , subscriptions = subscriptions
        }


type alias Model =
    { board : Board
    , time : Int
    , keyDown : Maybe Key
    , key : KeyState
    , minoState : Maybe MinoState
    , minoQueue : MinoQueue Mino
    , lines : Int
    , gameOver : Bool
    , seed : Seed
    }


type Msg
    = NoMsg
    | KeyDown Key
    | KeyUp
    | Tick Time.Posix
    | GetSeed Seed


type alias MinoState =
    { pos : Vec Int
    , rot : Int
    , mino : Mino
    , lifeTime : Maybe Int
    }


type Key
    = Z
    | X
    | Down
    | Up
    | Left
    | Right


type KeyState
    = KeyNotPressed
    | KeyDowned Key
    | KeyPressing Key


initMinoState : Mino -> MinoState
initMinoState mino =
    { pos = { x = 4, y = 0 }
    , rot = 0
    , mino = mino
    , lifeTime = Nothing
    }



-- INIT


init : () -> ( Model, Cmd Msg )
init _ =
    ( { board = Board.init
      , time = 0
      , keyDown = Nothing
      , key = KeyNotPressed
      , minoState = Nothing
      , minoQueue = MinoQueue.empty
      , lines = 0
      , gameOver = False
      , seed = Random.initialSeed 0
      }
    , Random.generate GetSeed Random.independentSeed
    )



-- VIEW


view : Model -> Html Msg
view model =
    H.div
        [ HA.css
            [ Css.width (Css.pct 100)
            , Css.height (Css.pct 100)
            ]
        ]
        [ H.div
            [ HA.css
                [ Css.width (Css.px ((cellLength * toFloat (boardWidth + 2)) + tetrisSideWidth))
                , Css.margin4 (Css.px 100) Css.auto Css.zero Css.auto
                ]
            ]
            [ viewTetris model
            , viewTetrisDescription model
            ]
        ]


viewTetris : Model -> Html Msg
viewTetris model =
    H.div
        [ HA.css
            [ Css.displayFlex
            ]
        ]
        [ viewTetrisBody model
        , viewTetrisSide model
        ]


viewTetrisSide : Model -> Html Msg
viewTetrisSide model =
    H.div
        [ HA.css
            [ Css.marginLeft (Css.px 10) ]
        ]
        [ viewScore model
        , viewMinoQueue model
        ]


viewScore : Model -> Html Msg
viewScore model =
    H.div []
        [ H.p
            [ HA.css
                [ Css.color
                    (if model.gameOver then
                        Css.hex "#f00"

                     else
                        Css.hex "#000"
                    )
                , Css.fontFamily Css.sansSerif
                ]
            ]
            [ H.text ("Score: " ++ String.fromInt model.lines) ]
        ]


viewMinoQueue : Model -> Html Msg
viewMinoQueue model =
    H.div
        [ HA.css
            [ Css.displayFlex
            , Css.flexDirection Css.column
            , Css.alignItems Css.center
            ]
        ]
        (List.indexedMap
            (\i x -> viewMinoIcon (i == 0) x)
            (MinoQueue.take 7 model.minoQueue)
        )


viewMinoIcon : Bool -> Mino -> Html Msg
viewMinoIcon isFirst mino =
    let
        info =
            Mino.info mino

        positions =
            Mino.positions info

        vb =
            Mino.viewBox mino

        viewBoxVal =
            String.join " "
                [ String.fromFloat (toFloat vb.x * cellIconLength)
                , String.fromFloat (toFloat vb.y * cellIconLength)
                , String.fromFloat (toFloat vb.width * cellIconLength)
                , String.fromFloat (toFloat vb.height * cellIconLength)
                ]
    in
    H.div
        [ HA.css
            [ Css.height (Css.px minoIconHeight)
            , if isFirst then
                Css.marginTop (Css.px 10.0)

              else
                Css.marginTop (Css.px 5.0)
            , Css.padding (Css.px 10.0)
            , if isFirst then
                Css.border3 (Css.px 1) Css.solid (Css.hex "#888")

              else
                Css.border Css.zero
            ]
        ]
        [ S.svg
            [ SA.css
                [ Css.width (Css.px (toFloat vb.width * cellIconLength))
                , Css.height (Css.px (toFloat vb.height * cellIconLength))
                ]
            , SA.viewBox viewBoxVal
            ]
            (List.map
                (\pos ->
                    S.g [ SA.transform (translate (Vec.mult cellIconLength <| Vec.toFloat pos)) ]
                        [ square cellIconLength Gray info.color ]
                )
                positions
            )
        ]


viewTetrisBody : Model -> Html Msg
viewTetrisBody model =
    S.svg
        [ SA.css
            [ Css.width (Css.px (cellLength * toFloat (boardWidth + 2)))
            , Css.height (Css.px (cellLength * toFloat (boardHeight + 1)))
            ]
        ]
        [ viewBoard model.minoState model.board
        , viewWall
        ]


viewTetrisDescription : Model -> Html Msg
viewTetrisDescription model =
    H.div
        []
        [ H.div
            [ HA.css [ Css.fontFamily Css.sansSerif ] ]
            [ H.p [] [ H.text "←  → : Move horizontally" ]
            , H.p [] [ H.text "↓ ↑: Soft and hard drop" ]
            , H.p [] [ H.text "Z X: Rotate" ]
            ]
        ]


viewBoard : Maybe MinoState -> Board -> Svg Msg
viewBoard state board =
    S.g
        [ SA.transform (translate { x = cellLength, y = 0.0 })
        ]
        (List.map
            (\{ pos, val } ->
                S.g
                    [ SA.transform (translate <| Vec.mult cellLength <| Vec.toFloat pos)
                    ]
                    [ viewCell val ]
            )
            (Board.toPositions <| Board.clip (Board.height - boardHeight) <| maskMinoIfPossible state board)
        )

toAbsolute : MinoState -> List (Vec Int)
toAbsolute { mino, pos, rot } =
  let
    ({ rotMax } as info) = Mino.info mino
    rotMod = modBy rotMax rot
  in
    List.map (Vec.add pos << Util.applyN rotMod Vec.rotate90) (Mino.positions info)


maskMinoIfPossible : Maybe MinoState -> Board -> Board
maskMinoIfPossible stateMay board =
    case stateMay of
        Just ({ mino } as state) ->
            let
                { color } = Mino.info mino
            in
            Board.putBlock (toAbsolute state) (Block color) board

        Nothing ->
            board


stateToPositions : MinoState -> List (Position Cell)
stateToPositions { rot, mino, pos } =
    let
        info =
            Mino.info mino
    in
    Mino.rotate (modBy info.rotMax rot) mino
        |> List.map (Vec.add pos)
        |> List.map (\v -> { pos = v, val = Block info.color })


viewCell : Cell -> Svg Msg
viewCell cell =
    case cell of
        Empty ->
            square cellLength Gray White

        Block col ->
            square cellLength Gray col


viewWall : Svg Msg
viewWall =
    S.g []
        [ viewVerticalWall 0
        , viewVerticalWall (boardWidth + 1)
        , viewHorizontalWall boardHeight
        ]


viewVerticalWall : Int -> Svg Msg
viewVerticalWall x =
    S.g [ SA.transform (translate { x = cellLength * toFloat x, y = 0.0 }) ]
        (List.map
            (\y ->
                S.g [ SA.transform (translate { x = 0, y = cellLength * toFloat y }) ]
                    [ square cellLength Black Black ]
            )
            (List.range 0 boardHeight)
        )


viewHorizontalWall : Int -> Svg Msg
viewHorizontalWall y =
    S.g [ SA.transform (translate { x = cellLength, y = cellLength * toFloat y }) ]
        (List.map
            (\x ->
                S.g [ SA.transform (translate { x = cellLength * toFloat x, y = 0 }) ]
                    [ square cellLength Black Black ]
            )
            (List.range 0 (boardWidth - 1))
        )


translate : Vec Float -> String
translate { x, y } =
    "translate(" ++ String.fromFloat x ++ "," ++ String.fromFloat y ++ ")"


square : Float -> Color -> Color -> Svg Msg
square len sColor fColor =
    S.rect
        [ SA.width (String.fromFloat len)
        , SA.height (String.fromFloat len)
        , SA.stroke (Color.toCode sColor)
        , SA.fill (Color.toCode fColor)
        ]
        []



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoMsg ->
            ( model, Cmd.none )

        KeyDown newKey ->
            ( { model | keyDown = Just newKey }
            , Cmd.none
            )

        KeyUp ->
            ( { model | keyDown = Nothing }
            , Cmd.none
            )

        Tick _ ->
            ( elapseTime model
                |> (\m -> { m | key = updateKeyState m.keyDown m.key })
                |> handleKey
                |> fallDown
                |> fixMinoIfPossible
                |> eraseLines
                |> nextMino
                |> checkGameOver
            , Cmd.none
            )

        GetSeed seed ->
            ( { model | seed = seed }
            , Cmd.none
            )


elapseTime : Model -> Model
elapseTime model =
    { model | time = model.time + 1 }


updateKeyState : Maybe Key -> KeyState -> KeyState
updateKeyState keyMay state =
    case keyMay of
        Just newKey ->
            case state of
                KeyDowned key ->
                    if key == newKey then
                        KeyPressing key

                    else
                        KeyDowned newKey

                KeyPressing key ->
                    if key == newKey then
                        KeyPressing key

                    else
                        KeyDowned newKey

                KeyNotPressed ->
                    KeyDowned newKey

        Nothing ->
            KeyNotPressed


handleKey : Model -> Model
handleKey ({ key, time, minoState, board } as model) =
    case key of
        KeyNotPressed ->
            model

        KeyDowned Z ->
            { model
                | minoState =
                    Maybe.map
                        (startLifeTime << rotateIfPossible -1 board)
                        minoState
            }

        KeyDowned X ->
            { model
                | minoState =
                    Maybe.map
                        (startLifeTime << rotateIfPossible 1 board)
                        minoState
            }

        KeyDowned Left ->
            { model | minoState = Maybe.map (moveIfPossible -1 0 board) minoState }

        KeyDowned Right ->
            { model | minoState = Maybe.map (moveIfPossible 1 0 board) minoState }

        KeyDowned Up ->
            { model
                | minoState = Maybe.map (setLifeTime 0 << hardDrop board) minoState
            }

        KeyPressing Down ->
            if modBy 5 time == 0 then
                { model | minoState = Maybe.map (moveIfPossible 0 1 board) minoState }

            else
                model

        _ ->
            model


rotateIfPossible : Int -> Board -> MinoState -> MinoState
rotateIfPossible dr board ({ rot, mino } as state) =
    let
        o1 =
            Mino.getOrient rot mino

        o2 =
            Mino.getOrient (rot + dr) mino

        kickList =
            Mino.getKickList o1 o2 mino
    in
    tryKickList kickList dr board state


hardDrop : Board -> MinoState -> MinoState
hardDrop board ({ pos } as state) =
    let
        newState =
            { state | pos = Vec.add (Vec 0 1) pos }
    in
    case Board.overlapped (stateToPositions newState) board of
        Just _ ->
            state

        Nothing ->
            hardDrop board newState


tryKickList : List (Vec Int) -> Int -> Board -> MinoState -> MinoState
tryKickList kickList dr board state =
    case kickList of
        [] ->
            state

        k :: ks ->
            let
                newState =
                    { state
                        | pos = Vec.add state.pos k
                        , rot = state.rot + dr
                    }
            in
            case Board.overlapped (stateToPositions newState) board of
                Just _ ->
                    tryKickList ks dr board state

                Nothing ->
                    newState


moveIfPossible : Int -> Int -> Board -> MinoState -> MinoState
moveIfPossible dx dy board ({ pos } as state) =
    let
        newState =
            { state | pos = Vec.add { x = dx, y = dy } pos }
    in
    case Board.overlapped (stateToPositions newState) board of
        Just _ ->
            state

        Nothing ->
            newState


fallDown : Model -> Model
fallDown ({ time, board, minoState } as model) =
    if modBy 15 time == 0 then
        { model | minoState = Maybe.map (moveIfPossible 0 1 board) minoState }

    else
        model


fixMinoIfPossible : Model -> Model
fixMinoIfPossible ({ minoState, board } as model) =
    case minoState of
        Just state ->
            let
                newState =
                    { state | pos = Vec.add { x = 0, y = 1 } state.pos }
            in
            case Board.overlapped (stateToPositions newState) board of
                Just _ ->
                    case state.lifeTime of
                        Just 0 ->
                            { model
                                | minoState = Nothing
                                , board = maskMinoIfPossible minoState board
                            }

                        Just t ->
                            { model | minoState = Just <| setLifeTime (t - 1) state }

                        Nothing ->
                            { model | minoState = Just <| startLifeTime state }

                Nothing ->
                    model

        Nothing ->
            model


nextMino : Model -> Model
nextMino ({ minoQueue, minoState } as model) =
    if not model.gameOver then
        case minoState of
            Just _ ->
                model

            Nothing ->
                case MinoQueue.takeOne minoQueue of
                    Nothing ->
                        let
                            ( minos, newSeed ) =
                                Random.step Mino.randoms model.seed

                            newQueue =
                                MinoQueue.insertToNext minos <| MinoQueue.swap minoQueue
                        in
                        { model
                            | minoQueue = newQueue
                            , seed = newSeed
                        }

                    Just ( mino, newQueue ) ->
                        { model
                            | minoQueue = newQueue
                            , minoState = Just <| initMinoState mino
                        }

    else
        model


setLifeTime : Int -> MinoState -> MinoState
setLifeTime t state =
    { state | lifeTime = Just t }


startLifeTime : MinoState -> MinoState
startLifeTime state =
    { state | lifeTime = Just 20 }


eraseLines : Model -> Model
eraseLines ({ board, lines } as model) =
    let
        ( newBoard, erasedLines ) =
            Board.eraseLines board
    in
    { model
        | board = newBoard
        , lines = lines + erasedLines
    }


checkGameOver : Model -> Model
checkGameOver model =
    case model.minoState of
        Just state ->
            case Board.overlapped (stateToPositions state) model.board of
                Just _ ->
                    { model | gameOver = True }

                Nothing ->
                    { model | minoState = Just state }

        Nothing ->
            model



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Browser.Events.onKeyDown (D.map toKey (D.field "key" D.string))
        , Browser.Events.onKeyUp (D.succeed KeyUp)
        , Browser.Events.onAnimationFrame Tick
        ]


toKey : String -> Msg
toKey key =
    case key of
        "z" ->
            KeyDown Z

        "x" ->
            KeyDown X

        "ArrowUp" ->
            KeyDown Up

        "ArrowDown" ->
            KeyDown Down

        "ArrowLeft" ->
            KeyDown Left

        "ArrowRight" ->
            KeyDown Right

        _ ->
            NoMsg
