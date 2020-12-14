module GameInteraction exposing (..)

import Html.Events.Extra.Touch as Touch
import Json.Decode as Decode


type KeyAction
    = KeyLeft
    | KeyRight
    | KeyDown
    | KeyUp
    | KeyOther


type alias TouchCoordinate =
    ( Float, Float )


type SwipeContinous
    = SwipeLeft
    | SwipeRight


type SwipeAndRelease
    = SwipeUpStop
    | SwipeDownStop
    | SwipeRightStop
    | SwipeLeftStop


toKeyAction : String -> KeyAction
toKeyAction string =
    case string of
        "ArrowLeft" ->
            KeyLeft

        "ArrowRight" ->
            KeyRight

        "ArrowDown" ->
            KeyDown

        "ArrowUp" ->
            KeyUp

        _ ->
            KeyOther


keyDecoder : (KeyAction -> msg) -> Decode.Decoder msg
keyDecoder toMsg =
    Decode.map toKeyAction (Decode.field "key" Decode.string)
        |> Decode.map toMsg


touchMove : Int -> Float -> Float -> Maybe SwipeContinous
touchMove tileSize newX startX =
    let
        relativeX =
            startX - newX

        deltaX =
            abs relativeX

        sensitivity =
            toFloat tileSize * 2
    in
    if relativeX > 0 && deltaX > sensitivity then
        Just SwipeLeft

    else if relativeX < 0 && deltaX > sensitivity then
        Just SwipeRight

    else
        Nothing


touchStop : TouchCoordinate -> TouchCoordinate -> Maybe SwipeAndRelease
touchStop ( newX, newY ) ( startX, startY ) =
    let
        relativeX =
            startX - newX

        deltaX =
            abs relativeX

        relativeY =
            startY - newY

        deltaY =
            abs relativeY
    in
    if deltaY > deltaX && relativeY < 0 then
        Just SwipeDownStop

    else if deltaY > deltaX && relativeY > 0 then
        Just SwipeUpStop

    else if deltaX > deltaY && relativeX > 0 then
        Just SwipeLeftStop

    else if deltaX > deltaY && relativeX < 0 then
        Just SwipeRightStop

    else
        Nothing


touchCoordinates : Touch.Event -> TouchCoordinate
touchCoordinates touchEvent =
    List.head touchEvent.changedTouches
        |> Maybe.map .screenPos
        |> Maybe.withDefault ( 0, 0 )
