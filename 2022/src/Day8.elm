module Day8 exposing (..)

import Array exposing (Array, get)
import Array.Extra as ArrEx
import List exposing (map)
import String exposing (fromChar, toInt)


solution : String -> ( String, String )
solution input =
    ( part1 input, part2 input )


part1 : String -> String
part1 input =
    let
        forest =
            forestFromStr input
    in
    forest
        |> Array.map
            (\row ->
                Array.map
                    (\tree ->
                        if isVisible forest tree then
                            1

                        else
                            0
                    )
                    row
            )
        |> Array.map (\row -> Array.foldl (+) 0 row)
        |> Array.foldl (+) 0
        |> String.fromInt


part2 : String -> String
part2 input =
    "<TODO>"


type alias Tree =
    -- ((x,y), val)
    ( ( Int, Int ), Int )


type alias Forest =
    Array (Array Tree)


forestFromStr : String -> Forest
forestFromStr input =
    String.lines input
        |> map String.toList
        |> map (\row -> map fromChar row)
        |> List.indexedMap
            (\rIndex row ->
                List.indexedMap
                    (\cIndex val -> ( ( cIndex, rIndex ), toInt val |> Maybe.withDefault 0 ))
                    row
            )
        |> map Array.fromList
        |> Array.fromList


getTree : Int -> Int -> Array (Array Tree) -> Maybe Tree
getTree x y arr =
    getRow y arr |> get x


getRow : Int -> Array (Array Tree) -> Array Tree
getRow n arr =
    get n arr
        |> Maybe.withDefault (Array.fromList [])


getCol : Int -> Array (Array Tree) -> Array Tree
getCol n arr =
    Array.map (\row -> get n row |> Maybe.withDefault ( ( -1, -1 ), -1 )) arr


isVisible : Forest -> Tree -> Bool
isVisible forest t =
    let
        ( north, south ) =
            getCol (tPos t |> Tuple.first) forest
                |> ArrEx.splitAt (tPos t |> Tuple.second)
                |> Tuple.mapSecond (\southTrees -> ArrEx.removeAt 0 southTrees)

        ( west, east ) =
            getRow (tPos t |> Tuple.second) forest
                |> ArrEx.splitAt (tPos t |> Tuple.first)
                |> Tuple.mapSecond (\eastTrees -> ArrEx.removeAt 0 eastTrees)
    in
    isVisibleFrom east t
        || isVisibleFrom west t
        || isVisibleFrom north t
        || isVisibleFrom south t


isVisibleFrom : Array Tree -> Tree -> Bool
isVisibleFrom trees tree =
    ArrEx.all (\( _, v ) -> v < tVal tree) trees


tPos : Tree -> ( Int, Int )
tPos ( p, _ ) =
    p


tVal : Tree -> Int
tVal ( _, v ) =
    v
