module Day4 exposing (..)

import Parser exposing ((|.), (|=), Parser, int, spaces, succeed, symbol)


solution : String -> ( String, String )
solution input =
    ( String.fromInt <| part1 input
    , String.fromInt <| part2 input
    )


part1 : String -> Int
part1 input =
    -- In how many assignment pairs does one fully contain the other?
    -- Example: Consider two assignment pairs "2-3,3-6\n1-9,2-5",
    -- in the second pair, assignment "1-9" fully contains "2-5".
    parseAssignments input
        |> List.map
            (\( elf1, elf2 ) ->
                if
                    (elf1.start <= elf2.start && elf1.end >= elf2.end)
                        || (elf2.start <= elf1.start && elf2.end >= elf1.end)
                then
                    1

                else
                    0
            )
        |> List.sum


part2 : String -> Int
part2 input =
    -- In how many assignment pairs does one fully or partially overlap the other?
    parseAssignments input
        |> List.map
            (\( elf1, elf2 ) ->
                if
                    (elf1.end >= elf2.start && elf1.start <= elf2.end)
                        || (elf1.start >= elf2.end && elf1.end <= elf2.start)
                then
                    1

                else
                    0
            )
        |> List.sum


type alias Assignment =
    { start : Int, end : Int }


parseAssignments : String -> List ( Assignment, Assignment )
parseAssignments input =
    let
        blankAssignment =
            { start = 0, end = 0 }

        make e =
            case Parser.run assignment e of
                Ok elf ->
                    elf

                Err _ ->
                    blankAssignment
    in
    String.lines input
        |> List.map (String.split ",")
        |> List.map
            (\team ->
                case team of
                    elf1 :: elf2 :: [] ->
                        ( make elf1, make elf2 )

                    _ ->
                        ( blankAssignment, blankAssignment )
            )


assignment : Parser Assignment
assignment =
    succeed Assignment
        |= int
        |. symbol "-"
        |= int
