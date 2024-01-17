module Day8 exposing (solution)

import Dict as D


solution : ( String -> String, String -> String )
solution =
    ( part1, part2 )


part1 : String -> String
part1 input =
    parse input
        |> stepsTo "ZZZ"
        |> String.fromInt


part2 : String -> String
part2 input =
    parse input
        |> simultaneousStepsTo "ZZZ"
        |> Debug.toString


type alias Map =
    { nodes : D.Dict String ( String, String )
    , directions : List Direction
    }


type Direction
    = L
    | R


stepsTo : String -> Map -> Int
stepsTo end map =
    let
        node x =
            D.get x map.nodes
                |> Maybe.withDefault ( "Err: No " ++ x ++ " in nodes.", "" )
                |> (\tpl -> ( x, tpl ))

        walkFrom : ( String, ( String, String ) ) -> Int -> List Direction -> Int
        walkFrom (( nodeName, ( lNode, rNode ) ) as currentNode) stepNum dirs =
            if nodeName == end then
                stepNum

            else
                case dirs of
                    step :: restOfSteps ->
                        case step of
                            L ->
                                walkFrom (node lNode) (stepNum + 1) restOfSteps

                            R ->
                                walkFrom (node rNode) (stepNum + 1) restOfSteps

                    [] ->
                        walkFrom currentNode stepNum map.directions
    in
    walkFrom (node "AAA") 0 map.directions


simultaneousStepsTo : String -> Map -> Int
simultaneousStepsTo end map =
    -- this one would take more time than I can afford
    0


parse : String -> Map
parse input =
    case String.lines input of
        directionStr :: _ :: lstNodeStr ->
            { nodes = toNodes lstNodeStr
            , directions = toInstruction directionStr
            }

        _ ->
            { nodes = D.empty, directions = [] }


toNodes : List String -> D.Dict String ( String, String )
toNodes directionStr =
    List.map (makePairAt " = ") directionStr
        |> List.map
            (\( node, adjacent ) ->
                ( node
                , String.dropLeft 1 adjacent
                    |> String.dropRight 1
                    |> makePairAt ", "
                )
            )
        |> D.fromList


toInstruction : String -> List Direction
toInstruction str =
    String.toList str
        |> List.filterMap
            (\c ->
                case c of
                    'R' ->
                        Just R

                    'L' ->
                        Just L

                    _ ->
                        Nothing
            )


makePairAt : String -> String -> ( String, String )
makePairAt loc str =
    case String.split loc str of
        fst :: snd :: [] ->
            ( fst, snd )

        _ ->
            ( "Err:makePairAt", "Err:makePairAt" )
