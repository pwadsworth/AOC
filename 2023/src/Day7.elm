module Day7 exposing (solution)

import Bitwise
import Dict as D
import List exposing (filterMap, indexedMap, map, sortBy, sum)
import List.Extra as LE


solution : ( String -> String, String -> String )
solution =
    ( part1, part2 )


part1 : String -> String
part1 input =
    parse PartOne input
        |> sortBy .cardValues
        |> indexedMap (\i h -> (i + 1) * h.bid)
        |> sum
        |> Debug.toString


part2 : String -> String
part2 input =
    parse PartTwo input
        |> sortBy .cardValues
        |> indexedMap (\i h -> (i + 1) * h.bid)
        |> sum
        |> Debug.toString


type Problem
    = PartOne
    | PartTwo


type alias Hand =
    { cards : String
    , cardValues : List Int
    , type_ : Type
    , bid : Int
    }


type Type
    = FiveOfaKind
    | FourOfaKind
    | FullHouse
    | ThreeOfaKind
    | TwoPair
    | OnePair
    | HighCard
    | Err


type alias Rank =
    Int


parse : Problem -> String -> List Hand
parse problem input =
    let
        toHand lst =
            case lst of
                handStr :: bid :: [] ->
                    { cards = handStr
                    , type_ = parserHand problem handStr
                    , bid = String.toInt bid |> Maybe.withDefault 0
                    , cardValues = String.toList handStr |> filterMap (cardValue problem)
                    }
                        |> (\hnd -> { hnd | cardValues = map (shiftBy hnd.type_) hnd.cardValues })

                _ ->
                    { cards = ""
                    , type_ = Err
                    , bid = 0
                    , cardValues = [ 0 ]
                    }

        shiftBy type_ value =
            case type_ of
                FiveOfaKind ->
                    Bitwise.shiftLeftBy 24 value

                FourOfaKind ->
                    Bitwise.shiftLeftBy 20 value

                FullHouse ->
                    Bitwise.shiftLeftBy 16 value

                ThreeOfaKind ->
                    Bitwise.shiftLeftBy 12 value

                TwoPair ->
                    Bitwise.shiftLeftBy 8 value

                OnePair ->
                    Bitwise.shiftLeftBy 4 value

                HighCard ->
                    value

                Err ->
                    0
    in
    String.lines input
        |> List.map (String.split " ")
        |> List.map toHand


parserHand : Problem -> String -> Type
parserHand problem handStr =
    let
        totype : D.Dict Char Int -> Type
        totype cardReps =
            case D.get 'J' cardReps of
                Nothing ->
                    if anyEqual 5 cardReps then
                        FiveOfaKind

                    else if anyEqual 4 cardReps then
                        FourOfaKind

                    else if anyEqual 3 cardReps && anyEqual 2 cardReps then
                        FullHouse

                    else if anyEqual 3 cardReps then
                        ThreeOfaKind

                    else if (List.length <| LE.findIndices (\( _, v ) -> v == 2) <| D.toList cardReps) == 2 then
                        TwoPair

                    else if anyEqual 2 cardReps then
                        OnePair

                    else if allEqual 1 cardReps then
                        HighCard

                    else
                        Err

                Just n ->
                    case problem of
                        PartOne ->
                            cardReps |> D.remove 'J' |> D.insert 'X' n |> totype

                        PartTwo ->
                            cardReps |> D.remove 'J' |> toTypeWithJokers n

        toTypeWithJokers : Int -> D.Dict Char Int -> Type
        toTypeWithJokers js cardReps =
            if js >= 4 then
                FiveOfaKind

            else if js == 3 && anyEqual 2 cardReps then
                FullHouse

            else if js == 3 && allEqual 1 cardReps then
                FourOfaKind

            else if js == 2 && anyEqual 3 cardReps then
                FiveOfaKind

            else if js == 2 && anyEqual 2 cardReps then
                FourOfaKind

            else if js == 2 && allEqual 1 cardReps then
                ThreeOfaKind

            else if js == 1 && anyEqual 4 cardReps then
                FiveOfaKind

            else if js == 1 && anyEqual 3 cardReps then
                FourOfaKind

            else if js == 1 && allEqual 2 cardReps then
                FullHouse

            else if js == 1 && anyEqual 2 cardReps then
                ThreeOfaKind

            else if js == 1 && allEqual 1 cardReps then
                TwoPair

            else
                OnePair
    in
    String.toList handStr
        |> LE.frequencies
        |> LE.unique
        |> D.fromList
        |> totype


cardValue : Problem -> Char -> Maybe Rank
cardValue problem c =
    let
        validChars =
            String.toList "23456789"
    in
    case c of
        'A' ->
            Just 14

        'K' ->
            Just 13

        'Q' ->
            Just 12

        'J' ->
            case problem of
                PartOne ->
                    Just 11

                PartTwo ->
                    Just 1

        'T' ->
            Just 10

        n ->
            if List.member n validChars then
                String.toInt (String.fromChar n)

            else
                Nothing


anyEqual : v -> D.Dict k v -> Bool
anyEqual n dic =
    D.foldl (\_ v acc -> (v == n) || acc) False dic


allEqual : v -> D.Dict k v -> Bool
allEqual n dic =
    D.foldl (\_ v acc -> (v == n) && acc) True dic
