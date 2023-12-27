module Day2 exposing (solution)

import Html.Attributes exposing (placeholder, type_)


solution : String -> ( String, String )
solution input =
    ( String.fromInt <| part1 input
    , String.fromInt <| part2 input
    )


part1 : String -> Int
part1 input =
    String.lines input
        |> List.map (\str -> ( String.left 1 str, String.right 1 str ))
        |> List.map score1
        |> List.sum


part2 : String -> Int
part2 input =
    String.lines input
        |> List.map (\str -> ( String.left 1 str, String.right 1 str ))
        |> List.map score2
        |> List.sum


type alias Game =
    ( String, String )


score1 : Game -> Int
score1 ( opponent, player ) =
    --   Y = 2; X = 1; Z = 0
    --   Rock = A X; Paper = B Y; Scissor = C Z
    let
        p =
            { wins = 6, draws = 3, lost = 0 }

        ( rock, paper, scissor ) =
            ( 1, 2, 3 )

        compareHands =
            case opponent of
                "A" ->
                    case player of
                        "Y" ->
                            p.wins

                        "X" ->
                            p.draws

                        _ ->
                            p.lost

                "B" ->
                    case player of
                        "Z" ->
                            p.wins

                        "Y" ->
                            p.draws

                        _ ->
                            p.lost

                "C" ->
                    case player of
                        "X" ->
                            p.wins

                        "Z" ->
                            p.draws

                        _ ->
                            p.lost

                _ ->
                    0
    in
    case player of
        "X" ->
            rock + compareHands

        "Y" ->
            paper + compareHands

        "Z" ->
            scissor + compareHands

        _ ->
            0


score2 : Game -> Int
score2 ( opponent, player ) =
    --   X -> lose, Y -> draw, Z -> win
    --   Rock = A X; Paper = B Y; Scissor = C Z
    let
        loseHand =
            case opponent of
                "A" ->
                    score1 ( opponent, "Z" )

                "B" ->
                    score1 ( opponent, "X" )

                "C" ->
                    score1 ( opponent, "Y" )

                _ ->
                    0

        drawHand =
            case opponent of
                "A" ->
                    score1 ( opponent, "X" )

                "B" ->
                    score1 ( opponent, "Y" )

                "C" ->
                    score1 ( opponent, "Z" )

                _ ->
                    0

        winHand =
            case opponent of
                "A" ->
                    score1 ( opponent, "Y" )

                "B" ->
                    score1 ( opponent, "Z" )

                "C" ->
                    score1 ( opponent, "X" )

                _ ->
                    0
    in
    case player of
        "X" ->
            loseHand

        "Y" ->
            drawHand

        "Z" ->
            winHand

        _ ->
            0
