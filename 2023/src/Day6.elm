module Day6 exposing (solution)


solution : ( String -> String, String -> String )
solution =
    ( part1, part2 )


type alias Race =
    { time : Float
    , distance : Float
    }


part1 : String -> String
part1 input =
    input
        |> toRaces
        |> List.map waysToWin
        |> List.map List.length
        |> List.product
        |> String.fromInt


part2 : String -> String
part2 input =
    input
        |> toRace
        |> solve
        |> Debug.toString


toRaces : String -> List Race
toRaces str =
    String.lines str
        |> List.map (String.split ":")
        |> List.filterMap List.tail
        |> List.concat
        |> List.map (String.split " ")
        |> List.map (List.filter (not << String.isEmpty))
        |> List.map (List.filterMap String.toFloat)
        |> (\lst ->
                case lst of
                    time :: distance :: [] ->
                        List.map2 (\t d -> { time = t, distance = d }) time distance

                    _ ->
                        []
           )


waysToWin : Race -> List Int
waysToWin race =
    let
        time =
            round race.time

        distance =
            round race.distance
    in
    List.range 0 time
        |> List.map (\t -> t * (time - t))
        |> List.filter (\d -> d > distance)


toRace : String -> Race
toRace str =
    String.lines str
        |> List.map (String.split ":")
        |> List.filterMap List.tail
        |> List.concat
        |> List.map (String.filter Char.isDigit)
        |> List.filterMap String.toFloat
        |> (\lst ->
                case lst of
                    time :: distance :: [] ->
                        { time = time, distance = distance }

                    _ ->
                        { time = -1, distance = -1 }
           )


solve : Race -> Int
solve race =
    let
        acc =
            -1

        xVertex =
            race.time / 2

        yVertex =
            xVertex ^ 2

        parabola x =
            acc * (x - xVertex) ^ 2 + yVertex

        solveForX y =
            -1 * sqrt ((y - yVertex) / acc) + xVertex

        lowerBound =
            solveForX race.distance

        roundingError =
            if isInt (parabola xVertex) then
                1

            else
                0

        isInt n =
            n == (round n |> toFloat)
    in
    round (2 * (xVertex - lowerBound)) - roundingError
