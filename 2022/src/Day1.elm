module Day1 exposing (solution)


solution : String -> ( String, String )
solution input =
    ( String.fromInt <| part1 input
    , String.fromInt <| part2 input
    )


part1 : String -> Int
part1 input =
    sumByElf input
        |> List.maximum
        |> Maybe.withDefault 0


part2 : String -> Int
part2 input =
    sumByElf input
        |> List.sort
        |> List.reverse
        |> List.take 3
        |> List.sum


sumByElf : String -> List Int
sumByElf input =
    String.split "\n\n" input
        |> List.map String.lines
        |> List.map (\l -> List.map (String.toInt >> Maybe.withDefault 0) l)
        |> List.map List.sum
