module Tests exposing (suite)

import Array exposing (Array)
import Day1
import Day2
import Day3
import Day4
import Day5
import Day6
import Day7
import Expect
import Test exposing (Test, describe, test)


testDay : Int
testDay =
    -- change this value to the day you want to test
    7


solutions : Array ( String -> String, String -> String )
solutions =
    {- Solution modules need to be in this array in day order,
       and must expose a "solution" tuple of functions for
       parts 1 and 2 for that day e.g. Day1.solution = (part1Function, part2Function). The functions must be String -> String.
    -}
    Array.fromList [ Day1.solution, Day2.solution, Day3.solution, Day4.solution, Day5.solution, Day6.solution, Day7.solution ]


suite : Test
suite =
    let
        ( f1, f2 ) =
            getFun solutions testDay

        ( input1, input2 ) =
            getInput inputs testDay

        ( r1, r2 ) =
            getResult expectedResults testDay
    in
    describe ("AOC23 Day " ++ String.fromInt testDay) <|
        [ test "Part 1 example" <|
            \_ -> f1 input1 |> Expect.equal r1
        , test "Part 2 example" <|
            \_ -> f2 input2 |> Expect.equal r2
        ]


expectedResults : Array ( String, String )
expectedResults =
    --- Results from problem prompts (part1, part2)
    Array.fromList
        [ ( "142", "281" )
        , ( "8", "2286" )
        , ( "4361", "467835" )
        , ( "13", "30" )
        , ( "35", "46" )
        , ( "288", "71503" )
        , ( "6440", "5905" )

        -- example
        , ( "last day part1 result", "last day part2 result" )
        ]


inputs : Array ( String, String )
inputs =
    --- Example inputs from problem prompts (part1, part2)
    Array.fromList
        [ ( """1abc2
pqr3stu8vwx
a1b2c3d4e5f
treb7uchet"""
          , """two1nine
eightwothree
abcone2threexyz
xtwone3four
4nineeightseven2
zoneight234
7pqrstsixteen"""
          )
        , ( """Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue
Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red
Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red
Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green"""
          , """Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue
Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red
Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red
Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green"""
          )
        , ( """467..114..
...*......
..35..633.
......#...
617*......
.....+.58.
..592.....
......755.
...$.*....
.664.598.."""
          , """467..114..
...*......
..35..633.
......#...
617*......
.....+.58.
..592.....
......755.
...$.*....
.664.598.."""
          )
        , ( """Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53
Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19
Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1
Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83
Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36
Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11"""
          , """Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53
Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19
Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1
Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83
Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36
Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11"""
          )
        , ( """seeds: 79 14 55 13

seed-to-soil map:
50 98 2
52 50 48

soil-to-fertilizer map:
0 15 37
37 52 2
39 0 15

fertilizer-to-water map:
49 53 8
0 11 42
42 0 7
57 7 4

water-to-light map:
88 18 7
18 25 70

light-to-temperature map:
45 77 23
81 45 19
68 64 13

temperature-to-humidity map:
0 69 1
1 0 69

humidity-to-location map:
60 56 37
56 93 4"""
          , """seeds: 79 14 55 13

seed-to-soil map:
50 98 2
52 50 48

soil-to-fertilizer map:
0 15 37
37 52 2
39 0 15

fertilizer-to-water map:
49 53 8
0 11 42
42 0 7
57 7 4

water-to-light map:
88 18 7
18 25 70

light-to-temperature map:
45 77 23
81 45 19
68 64 13

temperature-to-humidity map:
0 69 1
1 0 69

humidity-to-location map:
60 56 37
56 93 4"""
          )
        , ( """Time:      7  15   30
Distance:  9  40  200""", """Time:      7  15   30
Distance:  9  40  200""" )
        , ( """32T3K 765
T55J5 684
KK677 28
KTJJT 220
QQQJA 483""", """32T3K 765
T55J5 684
KK677 28
KTJJT 220
QQQJA 483""" )

        -- example
        , ( "last day part1 inputs"
          , "last day part2 inputs"
          )
        ]



--Helper Functions


getFun : Array ( String -> String, String -> String ) -> Int -> ( String -> String, String -> String )
getFun arr n =
    let
        funErr =
            "err: maybe you forgot to update the solutions array?"
    in
    Array.get (n - 1) arr
        |> Maybe.withDefault ( \_ -> funErr, \_ -> funErr )


getInput : Array ( String, String ) -> Int -> ( String, String )
getInput arr n =
    let
        err =
            "err: maybe you forgot to update the inputs array?"
    in
    Array.get (n - 1) arr
        |> Maybe.withDefault ( err, err )


getResult : Array ( String, String ) -> Int -> ( String, String )
getResult arr n =
    let
        err =
            "err: maybe you forgot to update the results array?"
    in
    Array.get (n - 1) arr
        |> Maybe.withDefault ( err, err )
