module Main exposing (Model, Msg(..), init, main, update, view)

-- UPDATE the imports Day# module to test against your solution

import Browser
import Browser.Dom exposing (Element)
import Day7 as Day
import Element exposing (Element, alignTop, centerX, column, el, fill, padding, paddingXY, rgb255, row, text, width)
import Element.Background as Background exposing (color)
import Element.Border exposing (rounded)
import Element.Font as Font
import Element.Input exposing (button)
import File exposing (File)
import File.Select as Select
import Html exposing (Html)
import Html.Attributes exposing (title)
import Task



-- MAIN


main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = always Sub.none
        }



-- MODEL


type alias Model =
    { fileName : String
    , input : String
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { fileName = "", input = "" }, Cmd.none )



-- UPDATE


type Msg
    = SelectInputClicked
    | FileSelected File
    | FileRead String
    | SelectedDay Int


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SelectInputClicked ->
            ( model, Select.file [] FileSelected )

        FileSelected file ->
            ( { model | fileName = File.name file }, Task.perform FileRead (File.toString file) )

        FileRead content ->
            ( { model | input = content }, Cmd.none )

        SelectedDay day ->
            ( model, Cmd.none )



-- VIEW


view : Model -> Html Msg
view model =
    Element.layout [] <|
        column
            [ width fill ]
            [ title
            , navBar model
            , mainArea model
            , footer model
            ]


title : Element msg
title =
    el [ Font.size 30, Font.italic, Font.semiBold, centerX, padding 10 ]
        (text "Advent of Code 2023 with Elm")


navBar : Model -> Element Msg
navBar model =
    row [ width fill, centerX, paddingXY 5 10 ]
        []


dayButtons : List Int -> List (Element Msg)
dayButtons daysSolved =
    daysSolved
        |> List.map
            (\d -> text ("day" ++ String.fromInt d))


mainArea : Model -> Element Msg
mainArea model =
    let
        ( part1, part2 ) =
            Day.solution
    in
    row [ width fill ]
        [ column [ alignTop ]
            [ row [ padding 5 ] [ text <| "Result Part 1: " ++ part1 model.input ]
            , row [ padding 5 ] [ text <| "Result Part 2: " ++ part2 model.input ]
            ]
        , inputsArea model
        ]


inputsArea : Model -> Element Msg
inputsArea model =
    column [ paddingXY 10 0, centerX ]
        [ button btnStyle
            { onPress = Just SelectInputClicked
            , label = text "Select File"
            }
        , el
            [ paddingXY 0 5 ]
            (text ("Input: " ++ model.fileName))
        , el
            [ Background.color black
            , Font.color offWhite
            , Font.family [ Font.monospace ]
            , Font.size 15
            , width fill
            ]
            (text model.input)
        ]


btnStyle : List (Element.Attribute msg)
btnStyle =
    [ rounded 7
    , Font.size 15
    , Font.underline
    , paddingXY 0 5
    , color offWhite
    ]


footer : Model -> Element Msg
footer model =
    Element.none


black : Element.Color
black =
    rgb255 0 0 0


darkGrey : Element.Color
darkGrey =
    rgb255 50 50 50


lightGrey : Element.Color
lightGrey =
    rgb255 150 150 150


offWhite : Element.Color
offWhite =
    rgb255 225 225 225
