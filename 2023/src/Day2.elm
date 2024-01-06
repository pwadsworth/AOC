module Day2 exposing (solution)


solution : ( String -> String, String -> String )
solution =
    ( part1, part2 )


type alias Game =
    ( Id, List Round )


type alias Id =
    Int


type alias Round =
    { r : Int
    , g : Int
    , b : Int
    }


part1 : String -> String
part1 input =
    gamesFromStr input
        |> Result.map (List.filter isValid)
        |> Result.map (List.foldl (\( id, _ ) -> (+) id) 0)
        |> Result.map String.fromInt
        |> (\r ->
                case r of
                    Err err ->
                        err

                    Ok str ->
                        str
           )


part2 : String -> String
part2 input =
    gamesFromStr input
        |> Result.map (List.map (\( _, rounds ) -> toPower rounds))
        |> Result.map List.sum
        |> Result.map String.fromInt
        |> (\r ->
                case r of
                    Err err ->
                        err

                    Ok str ->
                        str
           )


toPower : List Round -> Int
toPower rounds =
    let
        roundMax f =
            List.map f rounds |> List.maximum |> Maybe.withDefault 0
    in
    [ roundMax .r, roundMax .g, roundMax .b ]
        |> List.foldl (*) 1


gamesFromStr : String -> Result String (List Game)
gamesFromStr input =
    String.lines input
        |> List.map getGame
        |> maybesToResult


maybesToResult : List (Maybe a) -> Result String (List a)
maybesToResult maybes =
    let
        go : List (Maybe a) -> List a -> Int -> Result String (List a)
        go list acc index =
            case list of
                head :: tail ->
                    case head of
                        Just a ->
                            go tail (a :: acc) (index + 1)

                        Nothing ->
                            Err
                                ("Err: invalid input on line "
                                    ++ String.fromInt (index + 1)
                                )

                [] ->
                    Result.Ok (List.reverse acc)
    in
    go maybes [] 0


getGame : String -> Maybe Game
getGame gameStrs =
    case String.split ": " gameStrs of
        idStr :: roundsStr :: [] ->
            Just ( getId idStr, getRounds roundsStr )

        _ ->
            Nothing


getId : String -> Id
getId str =
    String.filter Char.isDigit str
        |> String.toInt
        |> Maybe.withDefault 0


getRounds : String -> List Round
getRounds str =
    String.split "; " str
        |> List.map getRound


getRound : String -> Round
getRound roundStr =
    { r = getColorVal "red" roundStr
    , g = getColorVal "green" roundStr
    , b = getColorVal "blue" roundStr
    }


getColorVal : String -> String -> Int
getColorVal color str =
    String.split ", " str
        |> List.filter (String.contains color)
        |> List.head
        |> Maybe.map (String.filter Char.isDigit)
        |> Maybe.andThen String.toInt
        |> Maybe.withDefault 0


isValid : Game -> Bool
isValid ( _, rounds ) =
    let
        maxVal =
            { r = 12, g = 13, b = 14 }
    in
    List.all
        (\round ->
            round.r <= maxVal.r && round.g <= maxVal.g && round.b <= maxVal.b
        )
        rounds
