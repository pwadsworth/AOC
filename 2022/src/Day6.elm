module Day6 exposing (solution)

import Set


solution : String -> ( String, String )
solution input =
    ( part1 input, part2 input )


part1 : String -> String
part1 input =
    -- Process the input 4 char at a time (rolling); when all 4 chars
    -- are different, report the index of the last one.
    nUniqueValuesPos 4 (String.toList input)
        |> String.fromInt


part2 : String -> String
part2 input =
    nUniqueValuesPos 14 (String.toList input)
        |> String.fromInt


nUniqueValuesPos : Int -> List comparable -> Int
nUniqueValuesPos n values =
    let
        differentCharsAt pos lst =
            if anyRepeat (List.take n lst) then
                differentCharsAt (pos + 1) (List.drop 1 lst)

            else
                ( pos + n, lst )
    in
    differentCharsAt 0 values
        |> Tuple.first


anyRepeat : List comparable -> Bool
anyRepeat lst =
    Set.fromList lst |> Set.size |> (\n -> List.length lst > n)
