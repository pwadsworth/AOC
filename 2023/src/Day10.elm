module Day10 exposing (..)

import Dict as D


solution : ( String -> String, String -> String )
solution =
    ( part1, part2 )


type alias Grid =
    D.Dict Pos Tile


type alias Pos =
    ( Int, Int )


type alias Tile =
    { pos : Pos, type_ : Char, dist : Maybe Int }


part1 : String -> String
part1 input =
    parse input
        |> calcDistances
        |> maxDistance
        |> String.fromInt


part2 : String -> String
part2 input =
    parse input
        |> calcDistances
        |> D.filter (\_ v -> v.dist /= Nothing)
        |> numEnclosedTiles
        |> Debug.toString


numEnclosedTiles : Grid -> Float
numEnclosedTiles grid =
    let
        b =
            D.size grid

        vertices =
            --FIX: Needs to return in clockwise order
            D.filter isVertice grid
                |> D.keys
                |> List.sortBy (\( x, y ) -> atan2 (toFloat x) (toFloat y))
                |> Debug.log "Vertices"

        a =
            let
                go vs ( acc1, acc2 ) =
                    case vs of
                        ( x1, y1 ) :: ( x2, y2 ) :: rest ->
                            go
                                (( x2, y2 ) :: rest)
                                ( (x1 * y2) + acc1, (y1 * x2) + acc2 )

                        _ ->
                            0.5 * toFloat (abs (acc1 - acc2))
            in
            go vertices ( 0, 0 )
    in
    -- Pick's Theorem
    a - (toFloat b / 2) + 1


isVertice : Pos -> Tile -> Bool
isVertice _ tile =
    List.member tile.type_ <| String.toList "SLJ7F"


parse : String -> Grid
parse str =
    String.lines str
        |> List.map String.toList
        |> List.indexedMap
            (\y col ->
                List.indexedMap
                    (\x elem -> ( ( x, y ), { pos = ( x, y ), type_ = elem, dist = Nothing } ))
                    col
            )
        |> List.concat
        |> D.fromList


calcDistances : Grid -> Grid
calcDistances grid =
    let
        addDistFrom : Tile -> Tile -> Grid -> Grid
        addDistFrom crtTile prvTile g =
            case crtTile.dist of
                Nothing ->
                    addDistFrom
                        (getTile (nxtPos crtTile prvTile.pos) g)
                        { crtTile | dist = Maybe.map ((+) 1) prvTile.dist }
                        (updateGrid crtTile.pos (Maybe.map ((+) 1) prvTile.dist) g)

                Just _ ->
                    g
    in
    addDistFrom (start grid) defaultTile grid


getTile : Pos -> Grid -> Tile
getTile pos grid =
    D.get pos grid |> Maybe.withDefault defaultTile


updateGrid : Pos -> Maybe Int -> Grid -> Grid
updateGrid pos dist grid =
    D.update pos (Maybe.map (\t -> { t | dist = dist })) grid


maxDistance : Grid -> Int
maxDistance grid =
    let
        maxDist _ tile acc =
            Maybe.map2
                (\d1 d2 ->
                    if d1 > d2 then
                        d1

                    else
                        d2
                )
                tile.dist
                acc
    in
    D.filter (\_ v -> v.dist /= Nothing) grid
        |> D.foldl maxDist (Just 0)
        |> Maybe.map (\d -> toFloat d / 2 |> round)
        |> Maybe.withDefault -1


start : Grid -> Tile
start grid =
    D.filter (\_ v -> v.type_ == 'S') grid
        |> D.toList
        |> List.head
        |> Maybe.withDefault ( ( -99, -99 ), defaultTile )
        |> Tuple.second


defaultTile : Tile
defaultTile =
    { dist = Just -1, pos = ( -999, -999 ), type_ = 'X' }


nxtPos : Tile -> ( Int, Int ) -> ( Int, Int )
nxtPos currentTile previousPos =
    let
        ( cX, cY ) =
            ( Tuple.first currentTile.pos
            , Tuple.second currentTile.pos
            )

        ( pX, pY ) =
            previousPos
    in
    case currentTile.type_ of
        '|' ->
            if pY > cY then
                ( cX, cY - 1 )

            else
                ( cX, cY + 1 )

        '-' ->
            if pX > cX then
                ( cX - 1, cY )

            else
                ( cX + 1, cY )

        'L' ->
            if pY >= cY then
                ( cX, cY - 1 )

            else
                ( cX + 1, cY )

        'J' ->
            if pY >= cY then
                ( cX, cY - 1 )

            else
                ( cX - 1, cY )

        '7' ->
            if pY <= cY then
                ( cX, cY + 1 )

            else
                ( cX - 1, cY )

        'F' ->
            if pY > cY then
                ( cX + 1, cY )

            else
                ( cX, cY + 1 )

        'S' ->
            ( cX + 1, cY )

        _ ->
            ( cX, cY )
