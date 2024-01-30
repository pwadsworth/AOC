module Day9 exposing (solution)


solution : ( String -> String, String -> String )
solution =
    ( part1, part2 )


type alias Report =
    List Int


type Target
    = NextVal
    | PreviousVal


part1 : String -> String
part1 input =
    parse input
        -- get the next value of each report and add them together
        |> List.foldl (\report acc -> acc + get NextVal report) 0
        |> Debug.toString


part2 : String -> String
part2 input =
    parse input
        -- get the previous value of each report and add them together
        |> List.foldl (\report acc -> acc + get PreviousVal report) 0
        |> Debug.toString


parse : String -> List Report
parse str =
    String.lines str
        |> List.map (String.split " ")
        |> List.map (List.filterMap String.toInt)


get : Target -> Report -> Int
get target report =
    let
        lastDiffByLevel diffAcc diffLevelAcc currentDiffLevel =
            -- recursively calculate diff between each value until all are equal
            case currentDiffLevel of
                n1 :: n2 :: ns ->
                    -- get differences in current level
                    lastDiffByLevel ((n2 - n1) :: diffAcc) diffLevelAcc (n2 :: ns)

                _ ->
                    if allAreEqual diffAcc then
                        -- differences are equal; return all diff levels.
                        case target of
                            NextVal ->
                                List.map List.reverse (diffAcc :: diffLevelAcc)

                            PreviousVal ->
                                diffAcc :: diffLevelAcc

                    else
                        -- differences are not equal; repeat process with the accumulated differences as input for next level of differences.
                        lastDiffByLevel [] (List.reverse diffAcc :: diffLevelAcc) (List.reverse diffAcc)
    in
    lastDiffByLevel [] [ report ] report
        -- Get first / last value for each diff level
        |> List.filterMap List.head
        |> (\diffs ->
                -- calculate the the next / previous value of the report
                case target of
                    NextVal ->
                        List.sum diffs

                    PreviousVal ->
                        List.foldl (-) 0 diffs
           )


allAreEqual : List a -> Bool
allAreEqual lst =
    case List.head lst of
        Just head ->
            List.all (\x -> x == head) lst

        Nothing ->
            True
