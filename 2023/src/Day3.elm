module Day3 exposing (..)

import Dict
import Html.Attributes exposing (coords)
import Json.Decode exposing (dict)
import Set


type alias Coord =
    ( Int, Int )


type alias Part =
    ( List Coord, Int )


type alias Symbol =
    ( Coord, Char )


type alias Schemamatic =
    { parts : List Part, symbols : List Symbol }


solution : ( String -> String, String -> String )
solution =
    ( part1, part2 )


part1 : String -> String
part1 input =
    -- Sum of part numbers
    toPartsSchematic input
        |> filter isPartAdjacentToSymbol
        |> (.parts >> List.map Tuple.second >> List.sum)
        |> Debug.toString


part2 : String -> String
part2 input =
    -- Sum of gear ratios
    toPartsSchematic input
        |> getGearRatios
        |> List.sum
        |> Debug.toString


toCoord : ( Int, String ) -> List Symbol
toCoord ( y, str ) =
    String.toList str
        |> List.indexedMap (\x char -> ( ( x, y ), char ))
        |> List.filter (\( _, char ) -> char /= '.')


toPartsSchematic : String -> Schemamatic
toPartsSchematic input =
    String.lines input
        |> List.indexedMap Tuple.pair
        |> List.concatMap toCoord
        |> (\lst ->
                { parts =
                    List.filter (\( _, c ) -> Char.isDigit c) lst
                        |> toRanges
                , symbols =
                    List.filter (\( _, c ) -> not (Char.isDigit c)) lst
                }
           )


toRanges : List Symbol -> List Part
toRanges list =
    toRangesHelp list [] [] ( -99, -99 ) []


toRangesHelp :
    List Symbol
    -> List Coord
    -> List Char
    -> Coord
    -> List ( List Coord, List Char )
    -> List Part
toRangesHelp lst coordAcc numAcc lastCoord rsltAcc =
    case lst of
        ( ( x1, y1 ), c1 ) :: (( ( x2, y2 ), _ ) as nxt) :: rest ->
            if x2 == x1 + 1 && y1 == y2 && ( x1, y1 ) /= lastCoord then
                toRangesHelp (nxt :: rest)
                    (( x1, y1 ) :: coordAcc)
                    (c1 :: numAcc)
                    ( x1, y1 )
                    rsltAcc

            else
                toRangesHelp (nxt :: rest)
                    []
                    []
                    ( -99, -99 )
                    (( ( x1, y1 ) :: coordAcc, c1 :: numAcc ) :: rsltAcc)

        [ ( ( x1, y1 ), c1 ) ] ->
            toRangesHelp []
                []
                []
                ( x1, y1 )
                (( ( x1, y1 ) :: coordAcc, c1 :: numAcc ) :: rsltAcc)

        [] ->
            List.reverse rsltAcc
                |> List.map (\( coords, numStr ) -> ( List.reverse coords, List.reverse numStr |> String.fromList |> String.toInt |> Maybe.withDefault -999 ))


isPartAdjacentToSymbol : List Symbol -> Part -> Bool
isPartAdjacentToSymbol symbols part =
    let
        coordsAdjToSymbols : List Coord
        coordsAdjToSymbols =
            List.concatMap (\( coord, _ ) -> coordsAdjTo coord) symbols

        partCoords =
            Tuple.first part
    in
    List.map (\n -> List.member n coordsAdjToSymbols) partCoords
        |> List.member True


coordsAdjTo : Coord -> List Coord
coordsAdjTo ( x, y ) =
    [ ( x - 1, y - 1 ), ( x, y - 1 ), ( x + 1, y - 1 ), ( x - 1, y ), ( x, y ), ( x + 1, y ), ( x - 1, y + 1 ), ( x, y + 1 ), ( x + 1, y + 1 ) ]


getGearRatios : Schemamatic -> List Int
getGearRatios sch =
    let
        gearAdjCoords : Dict.Dict Coord (List Coord)
        gearAdjCoords =
            List.filter (\( _, symbol ) -> symbol == '*') sch.symbols
                |> List.foldl (\( coord, _ ) -> updateOrIns coord (coordsAdjTo coord)) Dict.empty

        partsAdjToGears : Dict.Dict Coord (Set.Set Int)
        partsAdjToGears =
            Dict.map toAdjPartNumbers gearAdjCoords

        toAdjPartNumbers : Coord -> List Coord -> Set.Set Int
        toAdjPartNumbers _ gAdjCoords =
            sch.parts
                |> List.map
                    (\( pCoords, pNum ) ->
                        List.map
                            (\pCoord ->
                                if List.member pCoord gAdjCoords then
                                    pNum

                                else
                                    0
                            )
                            pCoords
                            |> List.filter (\n -> n /= 0)
                    )
                |> List.concat
                |> Set.fromList
    in
    partsAdjToGears
        |> Dict.filter (\_ v -> Set.size v == 2)
        |> Dict.values
        |> List.map (Set.toList >> List.product)


filter : (List Symbol -> Part -> Bool) -> Schemamatic -> Schemamatic
filter f sch =
    { sch | parts = List.filter (f sch.symbols) sch.parts }


updateOrIns : Coord -> List Coord -> Dict.Dict Coord (List Coord) -> Dict.Dict Coord (List Coord)
updateOrIns k v dict =
    if Dict.member k dict then
        Dict.update k (\_ -> Just v) dict

    else
        Dict.insert k v dict
