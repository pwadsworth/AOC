module Day4 exposing (solution)

import Parser exposing (..)


solution : ( String -> String, String -> String )
solution =
    ( part1, part2 )


type alias Card =
    { card : Int
    , winNums : List Int
    , cardNums : List Int
    , copies : Int
    }


part1 : String -> String
part1 input =
    parseCards input
        |> List.map cardPoints
        |> List.sum
        |> Debug.toString


part2 : String -> String
part2 input =
    parseCards input
        |> addCardCopies
        |> List.map .copies
        |> List.sum
        |> Debug.toString


cardPoints : Card -> Int
cardPoints card =
    let
        n =
            numMatches card
    in
    if n > 0 then
        2 ^ (n - 1)

    else
        0


numMatches : Card -> Int
numMatches card =
    List.map (\n -> List.member n card.winNums) card.cardNums
        |> List.filter (\c -> c == True)
        |> List.length


addCardCopies : List Card -> List Card
addCardCopies cards =
    let
        addCopies : List Card -> List Card -> List Card
        addCopies cards_ acc =
            case cards_ of
                c :: cs ->
                    addCopies
                        (addCopiesToNxt (numMatches c) cs c.copies)
                        (c :: acc)

                [] ->
                    acc

        addCopiesToNxt : Int -> List Card -> Int -> List Card
        addCopiesToNxt n remainingCards numPrevCopies =
            (List.take n remainingCards
                |> List.map (\c -> { c | copies = c.copies + numPrevCopies })
            )
                ++ List.drop n remainingCards
    in
    List.reverse <| addCopies cards []


parseCards : String -> List Card
parseCards input =
    String.lines input
        |> List.map (run cardParser)
        |> List.map Result.toMaybe
        |> List.filterMap identity


cardParser : Parser Card
cardParser =
    succeed Card
        |. keyword "Card"
        |. spaces
        |= int
        |= winNumsParser
        |= cardNumsParser
        |= succeed 1


winNumsParser : Parser (List Int)
winNumsParser =
    sequence
        { start = ":"
        , separator = ""
        , end = "|"
        , spaces = spaces
        , item = int
        , trailing = Optional
        }


cardNumsParser : Parser (List Int)
cardNumsParser =
    sequence
        { start = ""
        , separator = ""
        , end = ""
        , spaces = spaces
        , item = int
        , trailing = Optional
        }
