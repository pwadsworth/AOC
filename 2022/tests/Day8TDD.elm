module Day8TDD exposing (..)

import Array
import Day8 exposing (..)
import Expect
import Test exposing (Test, describe, skip, test)



{-
   This file is meant to encurage the use of test driven development.

   You can use a copy of it to get started defining tests for the functions
   you are creating for a particular problem. Remember to edit the DayX
   import statement above, and to expose the function you want to test.

   The overall results for each day example inputs are already included
   in Tests.elm.
-}


forest =
    -- [ [ 3, 0, 3, 7, 3 ]
    -- , [ 2, 5, 5, 1, 2 ]
    -- , [ 6, 5, 3, 3, 2 ]
    -- , [ 3, 3, 5, 4, 9 ]
    -- , [ 3, 5, 3, 9, 0 ] ]
    forestFromStr "30373\n25512\n65332\n33549\n35390"


suite : Test
suite =
    describe "Helper Functions Tests" <|
        [ test "getTree 3 3" <|
            \_ ->
                forest
                    |> getTree 3 3
                    |> Expect.equal (Just ( ( 3, 3 ), 4 ))
        , test "getTree 3 0" <|
            \_ ->
                forest
                    |> getTree 1 2
                    |> Expect.equal (Just ( ( 1, 2 ), 5 ))
        , test "getCol 1" <|
            \_ ->
                forest
                    |> getCol 1
                    |> Expect.equal
                        (Array.fromList
                            [ ( ( 1, 0 ), 0 )
                            , ( ( 1, 1 ), 5 )
                            , ( ( 1, 2 ), 5 )
                            , ( ( 1, 3 ), 3 )
                            , ( ( 1, 4 ), 5 )
                            ]
                        )
        , test "getRow 1" <|
            \_ ->
                forest
                    |> getRow 1
                    |> Expect.equal
                        (Array.fromList
                            [ ( ( 0, 1 ), 2 )
                            , ( ( 1, 1 ), 5 )
                            , ( ( 2, 1 ), 5 )
                            , ( ( 3, 1 ), 1 )
                            , ( ( 4, 1 ), 2 )
                            ]
                        )
        , test "isVisible 3 3" <|
            \_ ->
                isVisible forest (getTree 3 3 forest |> Maybe.withDefault ( ( 0, 0 ), 99 ))
                    |> Expect.equal False
        , test "isVisible 1 2" <|
            \_ ->
                isVisible forest (getTree 1 2 forest |> Maybe.withDefault ( ( 0, 0 ), 0 ))
                    |> Expect.equal True
        , test "isVisible 4 4" <|
            \_ ->
                isVisible forest (getTree 4 4 forest |> Maybe.withDefault ( ( 0, 0 ), 0 ))
                    |> Expect.equal True
        ]
