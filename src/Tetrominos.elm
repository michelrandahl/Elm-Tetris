module Tetrominos exposing (..)

import Collage exposing (Collage, filled, square, uniform)
import Collage.Layout exposing (align, base, horizontal, spacer, vertical)
import Color exposing (Color)
import Dict exposing (Dict)
import Random exposing (Generator)
import Utils exposing (..)


type alias TilePoint =
    ( Int, Int )


type alias Tetromino =
    { shape : List TilePoint
    , color : Color
    }


type Rotation
    = NoRotation
    | RotateLeft Rotation
    | RotateRight Rotation


type TetrominoType
    = I
    | O
    | T
    | J
    | L
    | S
    | Z


transposePoint : TilePoint -> TilePoint -> TilePoint
transposePoint ( locationX, locationY ) ( x, y ) =
    ( x + locationX, y + locationY )


transposeTetromino : TilePoint -> Tetromino -> Tetromino
transposeTetromino location ({ shape } as tetromino) =
    { tetromino | shape = List.map (\tilePoint -> transposePoint location tilePoint) shape }


mirror : List TilePoint -> List TilePoint
mirror =
    List.map (\( x, y ) -> ( -x, y ))


rotateRight : List TilePoint -> List TilePoint
rotateRight xs =
    let
        rotated =
            List.map (\( x, y ) -> ( y, -x )) xs

        maximumExtent =
            rotated
                |> List.map Tuple.second
                |> List.minimum
                |> Maybe.map abs
                |> Maybe.withDefault 0
    in
    List.map (Tuple.mapSecond ((+) maximumExtent)) rotated


rotateLeft : List TilePoint -> List TilePoint
rotateLeft xs =
    let
        rotated =
            List.map (\( x, y ) -> ( -y, x )) xs

        maximumExtent =
            rotated
                |> List.map Tuple.first
                |> List.minimum
                |> Maybe.map abs
                |> Maybe.withDefault 0
    in
    List.map (Tuple.mapFirst ((+) maximumExtent)) rotated


rotate : Rotation -> Tetromino -> Tetromino
rotate rotation ({ shape } as tetromino) =
    case rotation of
        NoRotation ->
            tetromino

        RotateLeft r ->
            rotate r { tetromino | shape = rotateLeft shape }

        RotateRight r ->
            rotate r { tetromino | shape = rotateRight shape }



-- TODO: make a general render module


renderTile : Int -> Maybe Color -> Collage msg
renderTile blockSize mColor =
    square (toFloat blockSize)
        |> filled (uniform (Maybe.withDefault Color.white mColor))


renderTetromino : Int -> Tetromino -> Collage msg
renderTetromino blockSize tetromino =
    let
        location =
            ( 1, 1 )

        width =
            6

        height =
            6

        { shape, color } =
            transposeTetromino location tetromino

        tetrominoArea =
            shape
                |> List.map (flip Tuple.pair color)
                |> Dict.fromList
    in
    List.repeat height (List.range 0 (width - 1))
        |> List.indexedMap
            (\y ->
                List.map
                    (flip Tuple.pair y
                        >> fromDictGet tetrominoArea
                        >> renderTile blockSize
                    )
                    >> horizontal
            )
        |> List.reverse
        |> vertical


o_Tetromino : Tetromino
o_Tetromino =
    { color = Color.yellow
    , shape = [ ( 0, 0 ), ( 1, 0 ), ( 1, 1 ), ( 0, 1 ) ]
    }


i_Tetromino : Tetromino
i_Tetromino =
    { color = Color.lightBlue
    , shape = [ ( 0, 0 ), ( 0, 1 ), ( 0, 2 ), ( 0, 3 ) ]
    }


t_Tetromino : Tetromino
t_Tetromino =
    { color = Color.purple
    , shape = [ ( 0, 0 ), ( 1, 0 ), ( 2, 0 ), ( 1, 1 ) ]
    }


l_Tetromino : Tetromino
l_Tetromino =
    { color = Color.orange
    , shape = [ ( 0, 0 ), ( 1, 0 ), ( 2, 0 ), ( 2, 1 ) ]
    }


j_Tetromino : Tetromino
j_Tetromino =
    { color = Color.blue
    , shape = [ ( 0, 0 ), ( 0, 1 ), ( 1, 0 ), ( 2, 0 ) ]
    }


s_Tetromino : Tetromino
s_Tetromino =
    { color = Color.green
    , shape = [ ( 0, 0 ), ( 1, 0 ), ( 1, 1 ), ( 2, 1 ) ]
    }


z_Tetromino : Tetromino
z_Tetromino =
    { color = Color.red
    , shape = [ ( 0, 1 ), ( 1, 1 ), ( 1, 0 ), ( 2, 0 ) ]
    }


createTetromino : TetrominoType -> Tetromino
createTetromino tetrominoType =
    case tetrominoType of
        I ->
            i_Tetromino

        O ->
            o_Tetromino

        T ->
            t_Tetromino

        J ->
            j_Tetromino

        L ->
            l_Tetromino

        S ->
            s_Tetromino

        Z ->
            z_Tetromino


randomTetrominoType : Generator TetrominoType
randomTetrominoType =
    Random.uniform I [ O, T, J, L, S, Z ]


randomRotation : Generator Rotation
randomRotation =
    Random.uniform
        NoRotation
        [ RotateLeft NoRotation
        , RotateLeft (RotateLeft NoRotation)
        , RotateLeft (RotateLeft (RotateLeft NoRotation))
        , RotateRight NoRotation
        , RotateRight (RotateRight NoRotation)
        , RotateRight (RotateRight (RotateRight NoRotation))
        ]


randomTetromino : Generator Tetromino
randomTetromino =
    Random.pair randomTetrominoType randomRotation
        |> Random.map
            (\( tetrominoType, rotation ) ->
                rotate rotation (createTetromino tetrominoType)
            )



-- Testing out the visualization of tetrominos and their rotations


hspace : Int -> Collage msg
hspace blockSize =
    spacer (toFloat blockSize) 0


vspace : Int -> Collage msg
vspace blockSize =
    spacer 0 (toFloat blockSize)


visualizeAllRotations : Int -> TetrominoType -> Collage msg
visualizeAllRotations blockSize tetrominoType =
    let
        tetromino =
            createTetromino tetrominoType
    in
    [ rotate (RotateLeft (RotateLeft (RotateLeft (RotateLeft NoRotation)))) tetromino
    , rotate (RotateLeft (RotateLeft (RotateLeft NoRotation))) tetromino
    , rotate (RotateLeft (RotateLeft NoRotation)) tetromino
    , rotate (RotateLeft NoRotation) tetromino
    , { tetromino | color = Color.black }
    , rotate (RotateRight NoRotation) tetromino
    , rotate (RotateRight (RotateRight NoRotation)) tetromino
    , rotate (RotateRight (RotateRight (RotateRight NoRotation))) tetromino
    , rotate (RotateRight (RotateRight (RotateRight (RotateRight NoRotation)))) tetromino
    ]
        |> List.map (renderTetromino blockSize)
        |> List.intersperse (hspace blockSize)
        |> List.map (align base)
        |> horizontal


allRotations : Int -> Collage msg
allRotations blockSize =
    [ I, O, T, J, L, S, Z ]
        |> List.map (visualizeAllRotations blockSize)
        |> List.intersperse (vspace blockSize)
        |> vertical
