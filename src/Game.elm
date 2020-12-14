module Game exposing (main)

import Browser
import Browser.Events exposing (onKeyDown)
import Collage exposing (Collage)
import Collage.Layout exposing (horizontal, vertical)
import Collage.Render exposing (svg)
import GameInteraction exposing (..)
import Html exposing (Html, div, text)
import Html.Attributes as Attributes exposing (class, style)
import Html.Events.Extra.Touch as Touch
import Matrix exposing (..)
import Random
import Tetrominos exposing (..)
import Time
import Utils exposing (..)


type GameState
    = GameOver
    | Running


type Scoring
    = SoftDrop
    | HardDrop
    | LinesCleared Int


type Msg
    = Keypress KeyAction
    | Tick Time.Posix
    | NewTetromino Tetromino
    | TouchStart TouchCoordinate
    | TouchMove TouchCoordinate
    | TouchCancel TouchCoordinate
    | TouchEnd TouchCoordinate


type alias Model =
    { matrix : Matrix
    , totalScore : Int
    , level : Int
    , linesToNextLevel : Int
    , hardDrop : Bool
    , tickCounter : Int
    , gameState : GameState
    , startTouchCoordinates : Maybe TouchCoordinate
    , tetromino : Tetromino
    , speed : Float
    }


evaluateScoring : Int -> Scoring -> Int
evaluateScoring level scoring =
    case scoring of
        SoftDrop ->
            1 * (level + 1)

        HardDrop ->
            2 * (level + 1)

        LinesCleared lines ->
            case lines of
                0 ->
                    0

                1 ->
                    40 * (level + 1)

                2 ->
                    80 * (level + 1)

                3 ->
                    300 * (level + 1)

                _ ->
                    1200 * (level + 1)


evaluateScorings : Int -> List Scoring -> Int
evaluateScorings level scorings =
    scorings
        |> List.map (evaluateScoring level)
        |> List.sum


timeTickSubscription : Model -> Sub Msg
timeTickSubscription { speed } =
    Time.every (max speed 10) Tick


renderMatrix : Tetromino -> Matrix -> Collage msg
renderMatrix tetromino { width, height, matrixArea, blockSize, tetrominoLocation } =
    let
        matrixWithTetrimino : MatrixArea
        matrixWithTetrimino =
            tetromino
                |> transposeTetromino tetrominoLocation
                |> addTetrominoToMatrix matrixArea
    in
    List.repeat height (List.range 0 (width - 1))
        |> List.indexedMap
            (\y ->
                List.map
                    (flip Tuple.pair y
                        >> fromDictGet matrixWithTetrimino
                        >> renderTile blockSize
                    )
                    >> horizontal
            )
        |> List.reverse
        |> vertical


initialModel : Model
initialModel =
    { matrix = initialMatrix
    , totalScore = 0
    , level = 0
    , linesToNextLevel = 10
    , tickCounter = 0
    , hardDrop = False
    , gameState = Running
    , startTouchCoordinates = Nothing
    , tetromino = o_Tetromino
    , speed = 600
    }


matrixView : GameState -> Tetromino -> Matrix -> Html Msg
matrixView gameState tetromino ({ blockSize, height, width } as matrix) =
    div
        [ Attributes.id "matrix"
        , style "height" (String.fromInt (height * blockSize) ++ "px")
        , style "min-width" (String.fromInt (width * blockSize) ++ "px")
        , Touch.onStart (touchCoordinates >> TouchStart)
        , Touch.onMove (touchCoordinates >> TouchMove)
        , Touch.onCancel (touchCoordinates >> TouchCancel)
        , Touch.onEnd (touchCoordinates >> TouchEnd)
        ]
        [ svg (renderMatrix tetromino matrix)
        , div
            [ Attributes.id "game-over" ]
            (case gameState of
                Running ->
                    []

                GameOver ->
                    [ text "GAME OVER!" ]
            )
        ]


view : Model -> Html Msg
view ({ tetromino, matrix, totalScore, level, tickCounter, gameState, startTouchCoordinates } as model) =
    let
        { blockSize } =
            matrix
    in
    div []
        [ div
            [ Attributes.id "game" ]
            [ matrixView gameState tetromino matrix
            , div [ Attributes.id "status" ]
                [ div [ class "status-text-field" ] [ text ("Score: " ++ String.fromInt totalScore) ]
                , div [ class "status-text-field" ] [ text ("Level: " ++ String.fromInt level) ]
                ]
            ]

        --, allRotations blockSize |> svg
        ]


createCmdHack : Msg -> Cmd Msg
createCmdHack msg =
    -- NOTE: Cmd does not expose any constructors
    Cmd.map (\_ -> msg) Cmd.none


evaluateGameTick : Model -> ( Model, Cmd Msg )
evaluateGameTick ({ tetromino, speed, tickCounter, matrix, hardDrop, level, linesToNextLevel, totalScore } as model) =
    let
        { tetrominoLocation } =
            matrix
    in
    if matrixTopReached matrix then
        ( { model | gameState = GameOver }, Cmd.none )

    else if isTouchDown tetromino matrix then
        let
            ( newMatrix, linesCleared ) =
                matrix
                    |> addTetrominoToClutter tetromino
                    |> clearAndCountFullLines

            newLinesToNextLevel =
                linesToNextLevel - linesCleared

            changeLevel =
                newLinesToNextLevel <= 0

            newLevel =
                level |> changeIf changeLevel ((+) 1)

            newSpeed =
                speed |> changeIf changeLevel ((+) -10)

            score =
                if hardDrop then
                    evaluateScorings newLevel [ HardDrop, LinesCleared linesCleared ]

                else
                    evaluateScorings newLevel [ SoftDrop, LinesCleared linesCleared ]
        in
        ( { model
            | tickCounter = tickCounter + 1
            , linesToNextLevel =
                newLinesToNextLevel
                    |> changeIf changeLevel ((+) 1 >> (*) 10)
            , level = newLevel
            , matrix = newMatrix
            , hardDrop = False
            , totalScore = totalScore + score
          }
        , Random.generate NewTetromino randomTetromino
        )

    else
        ( { model
            | tickCounter = tickCounter + 1
            , matrix = { matrix | tetrominoLocation = Tuple.mapSecond ((+) -1) tetrominoLocation }
          }
        , Cmd.none
        )


reactToSwipeStop : Model -> TouchCoordinate -> Maybe TouchCoordinate -> ( Model, Cmd Msg )
reactToSwipeStop ({ matrix, tetromino } as model) newCoordinates startTouchCoordinates =
    let
        { tetrominoLocation, width } =
            matrix
    in
    startTouchCoordinates
        |> Maybe.andThen (touchStop newCoordinates)
        |> Maybe.map
            (\verticalSwipe ->
                case verticalSwipe of
                    SwipeUpStop ->
                        let
                            rotated =
                                rotate (RotateRight NoRotation) tetromino

                            ( locationX, _ ) =
                                tetrominoLocation
                        in
                        if rotated.shape |> List.any (Tuple.first >> (+) locationX >> (\x -> x < 0 || x >= width)) then
                            model

                        else
                            { model | tetromino = rotated }

                    SwipeDownStop ->
                        { model
                            | matrix = forceTetrominoDown tetromino matrix
                            , hardDrop = True
                        }

                    SwipeLeftStop ->
                        { model | matrix = moveTetrominoLeft tetromino matrix }

                    SwipeRightStop ->
                        { model | matrix = moveTetrominoRight tetromino matrix }
            )
        |> Maybe.map (flip Tuple.pair Cmd.none)
        |> Maybe.withDefault ( model, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg ({ matrix, tetromino, gameState, startTouchCoordinates } as model) =
    let
        { tetrominoLocation, blockSize, width } =
            matrix
    in
    case gameState of
        GameOver ->
            ( { model | gameState = GameOver }, Cmd.none )

        Running ->
            case msg of
                Keypress KeyLeft ->
                    ( { model | matrix = moveTetrominoLeft tetromino matrix }, Cmd.none )

                Keypress KeyRight ->
                    ( { model | matrix = moveTetrominoRight tetromino matrix }, Cmd.none )

                Keypress KeyDown ->
                    ( { model | matrix = forceTetrominoDown tetromino matrix, hardDrop = True }, Cmd.none )

                Keypress KeyUp ->
                    let
                        rotated =
                            rotate (RotateRight NoRotation) tetromino

                        ( locationX, _ ) =
                            tetrominoLocation
                    in
                    if rotated.shape |> List.any (Tuple.first >> (+) locationX >> (\x -> x < 0 || x >= width)) then
                        ( model, Cmd.none )

                    else
                        ( { model | tetromino = rotated }
                        , Cmd.none
                        )

                Keypress KeyOther ->
                    ( model, Cmd.none )

                Tick _ ->
                    evaluateGameTick model

                NewTetromino newTetromino ->
                    ( { model
                        | tetromino = newTetromino
                        , matrix = { matrix | tetrominoLocation = initialTetrominoLocation }
                      }
                    , Cmd.none
                    )

                TouchStart coordinates ->
                    ( { model | startTouchCoordinates = Just coordinates }, Cmd.none )

                TouchMove ( newX, _ ) ->
                    startTouchCoordinates
                        |> Maybe.andThen (Tuple.first >> touchMove blockSize newX)
                        |> Maybe.map
                            (\horizontalSwipe ->
                                case horizontalSwipe of
                                    SwipeLeft ->
                                        ( { model | matrix = moveTetrominoLeft tetromino matrix }, Cmd.none )

                                    SwipeRight ->
                                        ( { model | matrix = moveTetrominoRight tetromino matrix }, Cmd.none )
                            )
                        |> Maybe.withDefault ( model, Cmd.none )

                TouchCancel newCoordinates ->
                    reactToSwipeStop model newCoordinates startTouchCoordinates

                TouchEnd newCoordinates ->
                    reactToSwipeStop model newCoordinates startTouchCoordinates


main : Program () Model Msg
main =
    Browser.element
        { init = \_ -> ( initialModel, Cmd.none )
        , view = view
        , update = update
        , subscriptions =
            \model ->
                Sub.batch
                    [ onKeyDown (keyDecoder Keypress)
                    , timeTickSubscription model
                    ]
        }
