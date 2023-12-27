module Day5 exposing (solution)

import Array exposing (Array)
import Char
import List.Extra
import Parser exposing (..)


type alias Model =
    { crates : Array (List Crate)
    , instructions : List Step
    , crane : Crane
    }


type alias Crate =
    { pos : Int
    , val : Char
    }


type alias Step =
    { qty : Int
    , from : Int
    , to : Int
    }


type Crane
    = CM9000
    | CM9001


solution : String -> ( String, String )
solution input =
    let
        model =
            case Parser.run fileParser input of
                Ok result ->
                    result

                Err _ ->
                    { instructions = [], crates = Array.empty, crane = CM9000 }

        processOutput arr =
            getTopCrates arr
                |> List.map .val
                |> String.fromList
    in
    ( do model |> processOutput
    , do { model | crane = CM9001 } |> processOutput
    )


do : Model -> Array (List Crate)
do model =
    let
        { instructions, crates, crane } =
            model
    in
    case instructions of
        [] ->
            crates
                |> Array.map (\stck -> List.filter (\c -> c.val /= ' ') stck)

        step :: rest ->
            do
                { model
                    | instructions = rest
                    , crates = execute crane step crates
                }


execute : Crane -> Step -> Array (List Crate) -> Array (List Crate)
execute crane step crates =
    let
        ( qty, from, to ) =
            ( step.qty, step.from - 1, step.to - 1 )

        ( movingCrates, newFromStack ) =
            Array.get from crates
                |> Maybe.map
                    (\stck ->
                        List.Extra.splitAt
                            (case crane of
                                CM9000 ->
                                    1

                                CM9001 ->
                                    qty
                            )
                            stck
                    )
                |> Maybe.withDefault ( [], [] )

        toStack : List Crate
        toStack =
            Array.get to crates |> Maybe.withDefault []

        newToStack : List Crate
        newToStack =
            movingCrates ++ toStack
    in
    if qty > 0 then
        Array.set from newFromStack crates
            |> Array.set to newToStack
            |> execute crane
                { step
                    | qty =
                        case crane of
                            CM9000 ->
                                qty - 1

                            CM9001 ->
                                0
                }

    else
        crates


getTopCrates : Array (List Crate) -> List Crate
getTopCrates stacks =
    Array.map List.head stacks
        |> Array.toList
        |> List.map (\c -> Maybe.withDefault { pos = -1, val = ' ' } c)


fileParser : Parser Model
fileParser =
    succeed Model
        |= (cratePileParser
                |> Parser.map organizeCrates
           )
        |. labels
        |= instructionsParser
        |= Parser.succeed CM9000


cratePileParser : Parser (List Crate)
cratePileParser =
    Parser.sequence
        { start = ""
        , separator = ""
        , end = ""
        , spaces = spaces
        , item = crateParser
        , trailing = Mandatory
        }


organizeCrates : List Crate -> Array (List Crate)
organizeCrates lst =
    let
        numStacks =
            List.Extra.maximumBy .pos lst |> Maybe.map .pos |> Maybe.withDefault 0 |> (+) 1

        initArr =
            Array.initialize numStacks (\_ -> [])

        updateArr crate arr =
            Array.set crate.pos (addToCrateStack crate <| Array.get crate.pos arr) arr

        addToCrateStack crate mStack =
            case mStack of
                Just stack ->
                    crate :: stack

                Nothing ->
                    []
    in
    List.foldl updateArr initArr lst
        |> Array.map List.reverse


labels : Parser ()
labels =
    succeed ()
        |. chompWhile (\c -> c == ' ' || Char.isDigit c)


crateParser : Parser Crate
crateParser =
    succeed Crate
        |. spaces
        |. symbol "["
        |= Parser.map (\n -> (n - 2) // 4) getCol
        |= marker
        |. symbol "]"


marker : Parser Char
marker =
    getChompedString (chompIf Char.isUpper)
        |> Parser.map (\c -> String.toList c |> List.head)
        |> andThen
            (\maybe ->
                case maybe of
                    Just c ->
                        succeed c

                    Nothing ->
                        problem "Invalid crate marker"
            )


instructionsParser : Parser (List Step)
instructionsParser =
    Parser.sequence
        { start = ""
        , separator = ""
        , end = ""
        , spaces = spaces
        , item = stepParser
        , trailing = Optional
        }


stepParser : Parser Step
stepParser =
    succeed Step
        |. symbol "move"
        |. spaces
        |= int
        |. spaces
        |. symbol "from"
        |. spaces
        |= int
        |. spaces
        |. symbol "to"
        |. spaces
        |= int
