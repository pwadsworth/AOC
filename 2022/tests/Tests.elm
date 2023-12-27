module Tests exposing (..)

import Array exposing (Array)
import Day1
import Day2
import Day3
import Day4
import Day5
import Day6
import Day7
import Day8
import Expect
import Test exposing (Test, describe, skip, test)


testDay : Int
testDay =
    8


solution : Array (String -> ( String, String ))
solution =
    {- Solution modules need to be in this array in day order, and must
       expose a "solution" function that takes the input string and returns
       the solutions for parts 1 and 2 as a tuple of strings
       e.g. Day1.solution input = ("part1 answer", "part2 answer")
    -}
    Array.fromList [ Day1.solution, Day2.solution, Day3.solution, Day4.solution, Day5.solution, Day6.solution, Day7.solution, Day8.solution ]


suite : Test
suite =
    let
        ( f1, f2 ) =
            getStr input testDay |> getFun solution testDay

        ( r1, r2 ) =
            getTpl result testDay
    in
    -- skip <|
    describe ("AOC22 Day " ++ String.fromInt testDay) <|
        [ test "Part 1 example" <|
            \_ -> f1 |> Expect.equal r1
        , test "Part 2 example" <|
            \_ -> f2 |> Expect.equal r2
        ]


result : Array ( String, String )
result =
    --- Results from problem prompts
    Array.fromList
        [ ( "24000", "45000" )
        , ( "15", "12" )
        , ( "157", "70" )
        , ( "2", "4" )
        , ( "CMZ", "MCD" )
        , ( "7", "19" )
        , ( "95437", "24933642" )
        , ( "21", "8" )
        ]


input : Array String
input =
    --- Example inputs from problem prompts
    Array.fromList
        [ "1000\n2000\n3000\n\n4000\n\n5000\n6000\n\n7000\n8000\n9000\n\n10000"
        , "A Y\nB X\nC Z"
        , "vJrwpWtwJgWrhcsFMMfFFhFp\njqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL\nPmmdzqPrVvPwwTWBwg\nwMqvLMZHhHMvwLHjbvcjnnSBnvTQFn\nttgJtRGJQctTZtZT\nCrZsJsPPZsGzwwsLwLmpwMDw"
        , "2-4,6-8\n2-3,4-5\n5-7,7-9\n2-8,3-7\n6-6,4-6\n2-6,4-8"
        , "    [D]    \n [N] [C]    \n [Z] [M] [P]\n 1   2   3 \n\n move 1 from 2 to 1\n move 3 from 1 to 3\n move 2 from 2 to 1\n move 1 from 1 to 2"
        , "mjqjpqmgbljsphdztnvjfqwrcgsmlb"
        , "$ cd /\n$ ls\ndir a\n14848514 b.txt\n8504156 c.dat\ndir d\n$ cd a\n$ ls\ndir e\n29116 f\n2557 g\n62596 h.lst\n$ cd e\n$ ls\n584 i\n$ cd ..\n$ cd ..\n$ cd d\n$ ls\n4060174 j\n8033020 d.log\n5626152 d.ext\n7214296 k"
        , "30373\n25512\n65332\n33549\n35390"
        ]



--Helper Functions


getFun : Array (a -> ( String, String )) -> Int -> a -> ( String, String )
getFun arr n =
    arr |> Array.get (n - 1) |> Maybe.withDefault (\_ -> ( "err", "err" ))


getStr : Array String -> Int -> String
getStr arr n =
    arr |> Array.get (n - 1) |> Maybe.withDefault ""


getTpl : Array ( String, String ) -> Int -> ( String, String )
getTpl arr n =
    arr |> Array.get (n - 1) |> Maybe.withDefault ( "err", "err" )
