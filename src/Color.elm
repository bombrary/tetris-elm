module Color exposing (..)


type Color
    = Purple
    | LightBlue
    | Blue
    | Red
    | Green
    | Yellow
    | Orange
    | Black
    | Gray
    | White


toCode : Color -> String
toCode color =
    case color of
        Purple ->
            "#75507b"

        LightBlue ->
            "#729fcf"

        Blue ->
            "#3465a4"

        Red ->
            "#cc0000"

        Green ->
            "#73d216"

        Yellow ->
            "#edd400"

        Orange ->
            "#f57900"

        Black ->
            "#000000"

        Gray ->
            "#d3d7cf"

        White ->
            "#FFFFFF"
