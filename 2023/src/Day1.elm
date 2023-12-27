module Day1 exposing (solution)


solution : ( String -> String, String -> String )
solution =
    ( part1, part2 )


part1 : String -> String
part1 input =
    String.lines input
        |> List.map (String.filter Char.isDigit)
        |> List.map (\str -> String.left 1 str ++ String.right 1 str)
        |> List.map (\n -> Maybe.withDefault 0 (String.toInt n))
        |> List.sum
        |> String.fromInt


replaceStrDigits : String -> String
replaceStrDigits str =
    let
        replace : String -> String -> String -> String
        replace before after string =
            String.join after (String.split before string)
    in
    str
        |> replace "one" "o1e"
        |> replace "two" "t2o"
        |> replace "three" "t3e"
        |> replace "four" "f4r"
        |> replace "five" "f5e"
        |> replace "six" "s6x"
        |> replace "seven" "s7n"
        |> replace "eight" "e8t"
        |> replace "nine" "n9e"


part2 : String -> String
part2 input =
    replaceStrDigits input
        |> part1
