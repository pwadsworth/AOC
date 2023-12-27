module Day5 exposing (solution)


solution : ( String -> String, String -> String )
solution =
    ( part1, part2 )


part1 : String -> String
part1 input =
    toAlamanac input
        |> applyAllMappings
        |> List.minimum
        |> Maybe.map String.fromInt
        |> Maybe.withDefault "Err"


part2 : String -> String
part2 input =
    toAlamanac2 input
        |> Debug.toString



-- |> applyAllMappings
-- |> List.minimum
-- |> Maybe.map String.fromInt
-- |> Maybe.withDefault "Err"


type alias Almanac =
    { seeds : Seeds
    , seedToSoil : List Range
    , soilToFert : List Range
    , fertToWatr : List Range
    , watrToLigt : List Range
    , ligtToTemp : List Range
    , tempToHumd : List Range
    , humdToloct : List Range
    }


type alias Almanac2 =
    { seeds : List ( Int, Int )
    , seedToSoil : List ( Int, Int, Int )
    , soilToFert : List ( Int, Int, Int )
    , fertToWatr : List ( Int, Int, Int )
    , watrToLigt : List ( Int, Int, Int )
    , ligtToTemp : List ( Int, Int, Int )
    , tempToHumd : List ( Int, Int, Int )
    , humdToloct : List ( Int, Int, Int )
    }


type alias Seeds =
    List Int


type alias Range =
    ( DestStart, SourceStart, Length )


type alias DestStart =
    Int


type alias SourceStart =
    Int


type alias Length =
    Int


type alias SeedStart =
    Int


type alias SeedEnd =
    Int


toAlamanac : String -> Almanac
toAlamanac input =
    let
        ( seeds, maps ) =
            case String.split "\n\n" input of
                seedStr :: mapStr ->
                    ( String.dropLeft 7 seedStr
                        |> String.split " "
                        |> List.filterMap String.toInt
                    , List.map (String.split "\n") mapStr
                        |> List.filterMap List.tail
                        |> List.map (List.map (String.split " "))
                        |> List.map (List.map (List.filterMap String.toInt))
                        |> List.map (List.filterMap toRange)
                    )

                _ ->
                    ( [], [] )

        toRange xs =
            case xs of
                destStart :: sourceStart :: length :: [] ->
                    Just ( destStart, sourceStart, length )

                _ ->
                    Nothing

        firstValToRange xs =
            List.concat <| List.take 1 xs
    in
    { seeds = seeds
    , seedToSoil = List.take 1 maps |> firstValToRange
    , soilToFert = List.drop 1 maps |> firstValToRange
    , fertToWatr = List.drop 2 maps |> firstValToRange
    , watrToLigt = List.drop 3 maps |> firstValToRange
    , ligtToTemp = List.drop 4 maps |> firstValToRange
    , tempToHumd = List.drop 5 maps |> firstValToRange
    , humdToloct = List.drop 6 maps |> firstValToRange
    }


toAlamanac2 : String -> Almanac2
toAlamanac2 input =
    let
        ( seeds, maps ) =
            case String.split "\n\n" input of
                seedStr :: mapStr ->
                    ( String.dropLeft 7 seedStr
                        |> String.split " "
                        |> List.filterMap String.toInt
                        |> makeSeedRanges
                    , List.map (String.split "\n") mapStr
                        |> List.filterMap List.tail
                        |> List.map (List.map (String.split " "))
                        |> List.map (List.map (List.filterMap String.toInt))
                        |> List.map (List.filterMap toRange)
                    )

                _ ->
                    ( [], [] )

        toRange xs =
            case xs of
                destStart :: sourceStart :: length :: [] ->
                    -- turn (deststart, sourceStart, length) into (rangeStart, rangeEnd, shift)
                    Just ( destStart, destStart + length - 1, destStart - sourceStart )

                _ ->
                    Nothing

        firstValToRange xs =
            List.concat <| List.take 1 xs
    in
    { seeds = seeds
    , seedToSoil = List.take 1 maps |> firstValToRange
    , soilToFert = List.drop 1 maps |> firstValToRange
    , fertToWatr = List.drop 2 maps |> firstValToRange
    , watrToLigt = List.drop 3 maps |> firstValToRange
    , ligtToTemp = List.drop 4 maps |> firstValToRange
    , tempToHumd = List.drop 5 maps |> firstValToRange
    , humdToloct = List.drop 6 maps |> firstValToRange
    }


applyAllMappings : Almanac -> List Int
applyAllMappings almnc =
    almnc.seeds
        |> applyMapping almnc.seedToSoil
        |> applyMapping almnc.soilToFert
        |> applyMapping almnc.fertToWatr
        |> applyMapping almnc.watrToLigt
        |> applyMapping almnc.ligtToTemp
        |> applyMapping almnc.tempToHumd
        |> applyMapping almnc.humdToloct


applyMapping : List Range -> Seeds -> List Int
applyMapping ranges seeds =
    let
        apply ranges_ seed =
            case ranges_ of
                ( destStart, sourceStart, length ) :: rest ->
                    if seed >= sourceStart && seed < sourceStart + length then
                        destStart + (seed - sourceStart)

                    else
                        apply rest seed

                [] ->
                    seed
    in
    List.map (apply ranges) seeds


applyAllMappings2 : Almanac -> List Int
applyAllMappings2 almnc =
    almnc.seeds
        |> applyMapping2 almnc.seedToSoil
        |> applyMapping2 almnc.soilToFert
        |> applyMapping2 almnc.fertToWatr
        |> applyMapping2 almnc.watrToLigt
        |> applyMapping2 almnc.ligtToTemp
        |> applyMapping2 almnc.tempToHumd
        |> applyMapping2 almnc.humdToloct


applyMapping2 : List Range -> Seeds -> List Int
applyMapping2 ranges seeds =
    let
        apply ranges_ seed =
            case ranges_ of
                ( destStart, sourceStart, length ) :: rest ->
                    if seed >= sourceStart && seed < sourceStart + length then
                        destStart + (seed - sourceStart)

                    else
                        apply rest seed

                [] ->
                    seed
    in
    List.map (apply ranges) seeds



-- makeSeedRanges : Almanac -> Almanac


makeSeedRanges : List Int -> List ( Int, Int )
makeSeedRanges seedMaps =
    let
        listToSeedRanges pairs acc =
            case pairs of
                start :: length :: rest ->
                    listToSeedRanges rest (( start, start + length ) :: acc)

                [ a ] ->
                    ( a, a ) :: acc

                [] ->
                    acc
    in
    listToSeedRanges seedMaps []
