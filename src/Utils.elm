module Utils exposing (..)

import Dict exposing (Dict)


{-| flip the parameters of a function
-}
flip : (a -> b -> c) -> b -> a -> c
flip f b a =
    f a b


fromDictGet : Dict comparable_index a -> comparable_index -> Maybe a
fromDictGet =
    flip Dict.get


memberInDict : Dict comparable_index a -> comparable_index -> Bool
memberInDict =
    flip Dict.member


changeIf : Bool -> (a -> a) -> a -> a
changeIf condition changeFun x =
    if condition then
        changeFun x

    else
        x


groupBy : (a -> comparable_b) -> List a -> List ( comparable_b, List a )
groupBy toComparable xs =
    let
        partition : List (List a) -> List a -> List (List a)
        partition result ys =
            case ( ys, result ) of
                ( z :: zs, resHead :: ress ) ->
                    case resHead of
                        r :: _ ->
                            if toComparable z == toComparable r then
                                partition ((z :: resHead) :: ress) zs

                            else
                                partition ([ z ] :: result) zs

                        [] ->
                            partition ([ z ] :: result) zs

                ( z :: zs, [] ) ->
                    partition [ [ z ] ] zs

                ( [], _ ) ->
                    result
    in
    List.sortBy toComparable xs
        |> partition []
        |> List.filterMap
            (\ys ->
                List.head ys |> Maybe.map (\y -> Tuple.pair (toComparable y) ys)
            )
