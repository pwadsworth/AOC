# Advent of Code in Elm Starter Files

Framework to get up and running quickly for Advent of Code (AoC) in Elm. 
You must have elm-live installed.
 
## Instructions

- Save the inputs from AoC to `/inputs` as `Day#.txt`, where "#" is a digit.

- Make a copy of `/src/Template.elm` as `/src/Day#.elm`, where "#" is a digit, and change the module's name accordingly.

- Use the copied template to develop your solutions exposing them as a `solution` tuple with **functions** for parts 1 and 2 of that day. The functions must be `(String -> String)`.

- In `Main.elm`, update line 7: `import Day# as Day` to the appropriate day.

- Run `elm-live src/Main.elm` to see your solutions at http://localhost:8000

## Testing framework 

The following instructions are optional but recommended. This allows you to use `elm-test` as you develop your solutions. You must have elm-test installed

In Test.elm:

- As you start a new AoC exercise, add the solutions (stubs) to the `Test.solutions` array in day order. 

    ```elm
    solutions : Array ( String -> String, String -> String )
    solutions = 
        Array.fromList [ Day1.solution
                       , Day2.solution ]
    ```
- Add a tuple with the expected results from problem prompts to the `results` array in day order.

    ```elm
    results : Array ( String, String )
    results = 
        Array.fromList [ ( "142", "281" )
                       , ( "8", "TBD" ) ]

- Add the inputs to the sample prompts to the `inputs` array in day order.

    ```elm
    inputs : Array ( String, String )
    inputs =
        Array.fromList
            [ ( "1abc2\npqr3stu8vwx\na1b2c3d4e5f\ntreb7uchet", "two1nine\neightwothree\nabcone2threexyz\nxtwone3four\n4nineeightseven2\nzoneight234\n7pqrstsixteen" )
            , ( "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green\nGame 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue\nGame 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red\nGame 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red\nGame 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green", "TBD" ) ]
    ```
- Run `elm-test --watch` to start your red-green-refactor journey!

## Dependencies

- elm-live
- elm-test (optional)
