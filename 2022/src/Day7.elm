module Day7 exposing (solution)

import Parser exposing (..)
import Tree exposing (singleton, tree)
import Tree.Zipper as TZ


type TermCmd
    = CD Argument
    | LS


type Argument
    = Home
    | UpOne
    | DownOne String


type Content
    = Dir String
    | File Int String String


type alias RawData =
    List ( List TermCmd, List Content )


solution : String -> ( String, String )
solution input =
    ( part1 input, part2 input )


part1 : String -> String
part1 input =
    Parser.run rawData input
        |> Result.withDefault [ ( [], [] ) ]
        |> toTree
        |> dirsWithSizeELT 100000
        |> List.map Tuple.second
        |> List.sum
        |> String.fromInt


part2 : String -> String
part2 input =
    "<Not implemented>"


toTree : List ( List TermCmd, List Content ) -> Tree.Tree Content
toTree raw =
    let
        toCtnt : ( List TermCmd, List Content ) -> TZ.Zipper Content -> TZ.Zipper Content
        toCtnt ( cmds, ctnts ) acc =
            List.foldl do ( acc, ctnts, "" ) cmds
                |> (\( a, _, _ ) -> a)

        do : TermCmd -> ( TZ.Zipper Content, List Content, String ) -> ( TZ.Zipper Content, List Content, String )
        do cmd ( acc, ctnts, current ) =
            case cmd of
                CD Home ->
                    ( TZ.root acc
                    , ctnts
                    , "/"
                    )

                CD UpOne ->
                    ( TZ.parent acc |> wDefaultZipper
                    , ctnts
                    , current
                    )

                CD (DownOne d) ->
                    ( TZ.findNext (\x -> x == Dir d) acc |> wDefaultZipper
                    , ctnts
                    , d
                    )

                LS ->
                    ( TZ.replaceTree (tree (Dir current) (List.map ctntToTree ctnts)) acc
                    , ctnts
                    , current
                    )

        wDefaultZipper =
            Maybe.withDefault (TZ.fromTree (singleton (Dir "defaultZipper")))
    in
    List.foldl toCtnt (TZ.fromTree <| Tree.singleton (Dir "toTree acc")) raw
        |> TZ.toTree


ctntToTree : Content -> Tree.Tree Content
ctntToTree ctnt =
    case ctnt of
        Dir nm ->
            Tree.singleton (Dir nm)

        File sz nm ext ->
            Tree.singleton (File sz nm ext)


dirsWithSizeELT : Int -> Tree.Tree Content -> List ( String, Int )
dirsWithSizeELT n ctnt =
    let
        dirsSizes : Content -> ( String, Int )
        dirsSizes c =
            case c of
                Dir name ->
                    ( name
                    , Maybe.map dirSize
                        (TZ.fromTree ctnt
                            |> TZ.findFromRoot (\x -> x == Dir name)
                            |> Maybe.map TZ.tree
                        )
                        |> Maybe.withDefault -1
                    )

                _ ->
                    ( "", -1 )
    in
    Tree.map dirsSizes ctnt
        |> Tree.flatten
        |> List.filter (\( _, sz ) -> sz <= n && sz >= 0)


dirSize : Tree.Tree Content -> Int
dirSize ctnt =
    let
        size content acc =
            case content of
                File sz _ _ ->
                    sz + acc

                Dir _ ->
                    acc
    in
    Tree.foldl size 0 ctnt


directory : Parser Content
directory =
    succeed Dir
        |. symbol "dir"
        |. spaces
        |= (chompUntilEndOr "\n" |> getChompedString)
        |. spaces


file : Parser Content
file =
    succeed File
        |= int
        |. spaces
        |= (chompWhile (\c -> c /= '.' && c /= '\n')
                |> getChompedString
           )
        |= (chompUntilEndOr "\n"
                |> getChompedString
                |> Parser.map (String.dropLeft 1)
           )
        |. spaces


command : Parser TermCmd
command =
    succeed identity
        |. symbol "$"
        |. spaces
        |= oneOf
            [ succeed CD
                |. symbol "cd"
                |. spaces
                |= argument
            , succeed LS
                |. symbol "ls"
            ]
        |. spaces


argument : Parser Argument
argument =
    oneOf
        [ succeed UpOne |. symbol ".."
        , succeed Home |. symbol "/"
        , succeed DownOne |= (chompUntilEndOr "\n" |> getChompedString)
        , problem "Bad argument"
        ]
        |. spaces


contents : Parser (List Content)
contents =
    let
        contentHelper revContent =
            oneOf
                [ succeed (\ctnt -> Loop (ctnt :: revContent))
                    |= file
                , succeed (\ctnt -> Loop (ctnt :: revContent))
                    |= directory
                , succeed ()
                    |> map (\_ -> Done (List.reverse revContent))
                ]
    in
    loop [] contentHelper


dataChunk : Parser ( List TermCmd, List Content )
dataChunk =
    succeed Tuple.pair
        |= commands
        |= contents


rawData : Parser (List ( List TermCmd, List Content ))
rawData =
    loop [] <| loopHelper dataChunk


commands : Parser (List TermCmd)
commands =
    loop [] <| loopHelper command


directories : Parser (List Content)
directories =
    loop [] <| loopHelper directory


files : Parser (List Content)
files =
    loop [] <| loopHelper file


loopHelper : Parser a -> List a -> Parser (Step (List a) (List a))
loopHelper parser revContent =
    oneOf
        [ succeed (\ctnt -> Loop (ctnt :: revContent))
            |= parser
        , checkNonEmpty revContent
            |> map (\_ -> Done (List.reverse revContent))
        ]


checkNonEmpty : List a -> Parser ()
checkNonEmpty list =
    if List.isEmpty list then
        problem "List is empty"

    else
        succeed ()


tst =
    """$ cd /
$ ls
1 1
1 2
dir a
dir b
dir c
$ cd a
$ ls
1 a1
1 a2
dir aa
$ cd aa
$ ls
dir aaa
1 aa1
1 aa2
$ cd aaa
$ cd aaaa
$ ls
1 aaaa1
1 aaaa2
$ cd /
$ cd b
$ ls
dir bb
1 b1
1 b2
$ cd bb
$ ls
dir bbb
$ cd bbb
$ ls
dir bbbb
$ cd bbbb
$ ls
dir bbbbb
$ cd bbbbb
$ ls
1 bbbbb1
1 bbbbb2
$ cd ..
$ cd ..
$ cd ..
$ cd ..
$ cd ..
$ cd c
$ ls
dir cc
$ cd cc
$ ls
dir ccc
$ cd ccc
$ ls
dir cccc
$ cd cccc
$ ls
dir ccccc
$ cd ccccc
$ ls
$ cd /
"""



{-
                       \
   1        a           b              c       1 2
   2   aa  1a 2a        bb 1b 2b       cc
   3   aaa 1aa 2aa      bbb            ccc
   4   aaaa             bbbb           cccc
   5   aaaa1 aaaa2      bbbbb          ccccc
   6                    bbbbb1 bbbbb2

   sz = 12
-}
