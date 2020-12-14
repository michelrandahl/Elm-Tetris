module Matrix exposing (..)

import Color exposing (Color)
import Dict exposing (Dict)
import List.Extra as ListExtra
import Tetrominos exposing (..)
import Utils exposing (..)


type alias MatrixArea =
    Dict TilePoint Color


type alias Matrix =
    { matrixArea : MatrixArea
    , lineCounts : Dict Int Int
    , height : Int
    , width : Int
    , blockSize : Int
    , tetrominoLocation : TilePoint

    --, currentTetromino : Tetromino
    }


initialTetrominoLocation : TilePoint
initialTetrominoLocation =
    ( 4, 20 )


makeEmptyMatrix : Int -> Int -> Int -> Matrix
makeEmptyMatrix blockSize height width =
    { matrixArea =
        Dict.empty
    , lineCounts = Dict.empty
    , height = height
    , width = width
    , blockSize = blockSize
    , tetrominoLocation = initialTetrominoLocation

    --, currentTetromino = squareTetromino
    }


initialMatrix : Matrix
initialMatrix =
    makeEmptyMatrix 25 20 10


addTetrominoToMatrix : MatrixArea -> Tetromino -> MatrixArea
addTetrominoToMatrix matrixArea { shape, color } =
    shape
        |> List.map (flip Tuple.pair color)
        |> Dict.fromList
        |> Dict.union matrixArea


addTetrominoToClutter : Tetromino -> Matrix -> Matrix
addTetrominoToClutter tetromino ({ matrixArea, tetrominoLocation, lineCounts } as matrix) =
    let
        transposedTetromino : Tetromino
        transposedTetromino =
            transposeTetromino tetrominoLocation tetromino

        tetrominoLineCounts : List ( Int, Int )
        tetrominoLineCounts =
            transposedTetromino
                |> .shape
                |> groupBy Tuple.second
                |> List.map (Tuple.mapSecond List.length)

        updatedLineCounts : Dict Int Int
        updatedLineCounts =
            tetrominoLineCounts
                |> List.foldl
                    (\( lineNumber, count ) ->
                        -- TODO: in the tutorial, introduce bug here by only using `Dict.update` and 'forgetting' to add if not exists
                        -- introduce the debugger
                        -- and introduce Debug.log
                        -- Dict.update lineNumber (Maybe.map ((+) count))
                        Dict.update lineNumber (Maybe.withDefault 0 >> (+) count >> Just)
                    )
                    lineCounts
    in
    { matrix
        | matrixArea = addTetrominoToMatrix matrixArea transposedTetromino
        , lineCounts = updatedLineCounts
    }


moveTetrominoLeft : Tetromino -> Matrix -> Matrix
moveTetrominoLeft tetromino ({ tetrominoLocation, matrixArea } as matrix) =
    if Tuple.first tetrominoLocation == 0 then
        matrix

    else
        let
            leftOfTetrominoLocation =
                Tuple.mapFirst ((+) -1) tetrominoLocation

            moveIsLegal =
                tetromino
                    |> .shape
                    |> List.map (transposePoint leftOfTetrominoLocation)
                    |> List.any (memberInDict matrixArea)
                    |> not
        in
        if moveIsLegal then
            { matrix | tetrominoLocation = leftOfTetrominoLocation }

        else
            matrix


moveTetrominoRight : Tetromino -> Matrix -> Matrix
moveTetrominoRight tetromino ({ tetrominoLocation, width, matrixArea } as matrix) =
    let
        rightOfTetrominoLocation =
            Tuple.mapFirst ((+) 1) tetrominoLocation

        locatedTetrominoPoints : List TilePoint
        locatedTetrominoPoints =
            tetromino
                |> .shape
                |> List.map (transposePoint rightOfTetrominoLocation)
    in
    if List.any (Tuple.first >> (<=) width) locatedTetrominoPoints then
        matrix

    else if List.any (memberInDict matrixArea) locatedTetrominoPoints then
        matrix

    else
        { matrix | tetrominoLocation = rightOfTetrominoLocation }


nearestClutter : Tetromino -> Matrix -> Maybe TilePoint
nearestClutter tetromino { tetrominoLocation, matrixArea } =
    let
        transposedTetrominoPoints : List TilePoint
        transposedTetrominoPoints =
            tetromino
                |> .shape
                |> List.map (transposePoint tetrominoLocation)

        lowestTetrominoTiles : List TilePoint
        lowestTetrominoTiles =
            transposedTetrominoPoints
                |> List.map Tuple.first
                |> ListExtra.unique
                |> List.filterMap
                    (\x ->
                        transposedTetrominoPoints
                            |> List.filter (Tuple.first >> (==) x)
                            |> ListExtra.minimumBy Tuple.second
                    )
    in
    lowestTetrominoTiles
        |> List.filterMap
            (\( x, y ) ->
                List.range 0 (y - 1)
                    |> List.reverse
                    |> ListExtra.find (Tuple.pair x >> memberInDict matrixArea)
                    |> Maybe.map (Tuple.pair x)
            )
        |> ListExtra.maximumBy Tuple.second


forceTetrominoDown : Tetromino -> Matrix -> Matrix
forceTetrominoDown tetromino ({ tetrominoLocation } as matrix) =
    let
        mClutter =
            nearestClutter tetromino matrix
    in
    case mClutter of
        Just ( _, y ) ->
            { matrix | tetrominoLocation = Tuple.mapSecond (always (y + 1)) tetrominoLocation }

        Nothing ->
            { matrix | tetrominoLocation = Tuple.mapSecond (always 0) tetrominoLocation }


isTouchDown : Tetromino -> Matrix -> Bool
isTouchDown tetromino { matrixArea, tetrominoLocation } =
    if Tuple.second tetrominoLocation == 0 then
        True

    else
        tetromino
            |> .shape
            |> List.map (transposePoint tetrominoLocation)
            |> List.any
                (Tuple.mapSecond ((+) -1)
                    >> fromDictGet matrixArea
                    >> Maybe.map (always True)
                    >> Maybe.withDefault False
                )


matrixTopReached : Matrix -> Bool
matrixTopReached { matrixArea, height, width } =
    List.range 0 (width - 1)
        |> List.any (flip Tuple.pair (height - 1) >> memberInDict matrixArea)


refreshLineCounts : MatrixArea -> Dict Int Int
refreshLineCounts matrixArea =
    matrixArea
        |> Dict.keys
        |> groupBy Tuple.second
        |> List.map (Tuple.mapSecond List.length)
        |> Dict.fromList


clearAndCountFullLines : Matrix -> ( Matrix, Int )
clearAndCountFullLines ({ width, lineCounts } as matrix) =
    let
        fullRows : List Int
        fullRows =
            lineCounts
                |> Dict.toList
                |> List.filter (Tuple.second >> (==) width)
                |> List.map Tuple.first
    in
    if List.isEmpty fullRows then
        ( matrix, 0 )

    else
        let
            wipeRow : Int -> MatrixArea -> MatrixArea
            wipeRow rowIndex matrixArea =
                List.range 0 (width - 1)
                    |> List.foldl (flip Tuple.pair rowIndex >> Dict.remove) matrixArea

            fixClutterAboveRow : Int -> MatrixArea -> MatrixArea
            fixClutterAboveRow rowIndex matrixArea =
                Dict.toList matrixArea
                    |> List.filter (Tuple.first >> Tuple.second >> (<) rowIndex)
                    |> List.sortBy (Tuple.first >> Tuple.second)
                    |> List.foldl
                        (\( tilePoint, color ) ->
                            let
                                newTilePoint =
                                    Tuple.mapSecond ((+) -1) tilePoint
                            in
                            Dict.remove tilePoint
                                >> Dict.insert newTilePoint color
                        )
                        matrixArea

            updateRow : Int -> MatrixArea -> MatrixArea
            updateRow rowIndex =
                wipeRow rowIndex >> fixClutterAboveRow rowIndex

            updatedMatrixArea : MatrixArea
            updatedMatrixArea =
                List.foldr updateRow matrix.matrixArea fullRows
        in
        ( { matrix | matrixArea = updatedMatrixArea, lineCounts = refreshLineCounts updatedMatrixArea }
        , List.length fullRows
        )
