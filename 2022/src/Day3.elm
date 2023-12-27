module Day3 exposing (solution)

import List.Extra as LE
import Set exposing (Set, empty)


solution : String -> ( String, String )
solution input =
    ( String.fromInt <| part1 input
    , String.fromInt <| part2 input
    )


part1 : String -> Int
part1 input =
    prioritize input
        |> separatePockets
        |> List.map LE.unique
        |> List.map List.sum
        |> List.sum


prioritize : String -> List (List Int)
prioritize input =
    {--Transforms input string so that:
Lowercase Chars a through z become priorities 1 through 26.
Uppercase Chars A through Z become priorities 27 through 52.
Returns one list of priorities per line of input.
--}
    let
        toPriority n =
            if n >= 65 && n <= 122 then
                if n >= 97 then
                    n - 96

                else
                    n - 38

            else
                0
    in
    String.lines input
        |> List.map String.toList
        |> List.map (\l -> List.map Char.toCode l)
        |> List.map (\l -> List.map toPriority l)


separatePockets : List (List Int) -> List (List Int)
separatePockets priorities =
    List.map (\elfInv -> LE.splitAt (List.length elfInv // 2) elfInv) priorities
        |> List.map
            (\( lPocket, rPocket ) ->
                List.filter (\item -> List.member item lPocket) rPocket
            )


part2 : String -> Int
part2 input =
    prioritize input
        |> toGroupsOf 3
        |> List.map findCommonItems
        |> List.concatMap Set.toList
        |> List.sum


findCommonItems : List (Set comparable) -> Set comparable
findCommonItems lst =
    case lst of
        [] ->
            Set.empty

        a :: [] ->
            a

        a :: b :: r ->
            findCommonItems (Set.intersect a b :: r)


toGroupsOf : Int -> List (List comparable) -> List (List (Set comparable))
toGroupsOf n lst =
    List.map Set.fromList lst
        |> LE.groupsOf n
