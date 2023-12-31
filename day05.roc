app "AoC"
    packages {
        pf: "https://github.com/roc-lang/basic-cli/releases/download/0.7.0/bkGby8jb0tmZYsy2hg1E_B2QrCgcSTxdUlHtETwm5m4.tar.br",
    }
    imports [pf.Stdout, pf.Task.{ Task }, "input/day05.txt" as input : Str]
    provides [main] to pf

main : Task {} *
main =
    printAnswers (part1 input) (part2 input)

printAnswers = \p1, p2 ->
    Stdout.line "part1: \(p1)\npart2: \(p2)"

testInput1 =
    """
    seeds: 79 14 55 13

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
    56 93 4
    """

part1 = \in ->
    numberList = in |> Str.split "\n\n"  
    seeds = numberList |> getForSure 0 |> Str.split ": " |> getForSure 1 |> Str.split " " |> List.keepOks Str.toI64
    mappingGroups =
        mappingInfo <- numberList |> List.dropFirst 1 |> List.map
        mappingLine <- Str.split mappingInfo ":\n" |> getForSure 1 |> Str.split "\n" |> List.map
        mappingNums = Str.split mappingLine " " |> List.keepOks Str.toI64
        when mappingNums is
            [to, from, len] ->

                { startFrom: from, endBefore: from + len, mapWithOffset: to - from }

            _ -> crash "mapping numbers need to come in groups of 3"

    locations = List.map seeds \seed -> applyMappingGroups seed mappingGroups
    List.min locations |> Result.withDefault 0 |> Num.toStr

expect part1 testInput1 == "35"

applyMappingGroups = \seed, mappingGroups ->
    List.walk mappingGroups seed \source, mappings ->
        List.walkUntil mappings source \s, { startFrom, endBefore, mapWithOffset } ->
            if startFrom <= s && s < endBefore then
                Break (s + mapWithOffset)
            else
                Continue s

testInput2 = testInput1

part2 = \in ->
    numberList = in |> Str.split "\n\n" 
    seedsRaw =
        numberList
        |> getForSure 0
        |> Str.split ": "
        |> getForSure 1
        |> Str.split " "
        |> List.keepOks Str.toI64
        |> List.chunksOf 2
        |> List.map \pair ->
            when pair is
                [from, len] -> {startsFrom: from, endsBefore: from + len}
                _ -> crash "seeds should comes in pairs"
    seeds = List.map seedsRaw Unchanged
    
    mappingGroups =
        mappingInfo <- numberList |> List.dropFirst 1 |> List.map
        mappingLine <- Str.split mappingInfo ":\n" |> getForSure 1 |> Str.split "\n" |> List.map
        mappingNums = Str.split mappingLine " " |> List.keepOks Str.toI64
        when mappingNums is
            [to, from, len] -> { startFrom: from, endBefore: from + len, mapWithOffset: to - from }
            _ -> crash "mapping numbers need to come in groups of 3"

    locationRanges = applyMappingGroupsToRanges seeds mappingGroups |> getStartOfRange 
    locationRanges |> List.min |> Result.withDefault 0 |> Num.toStr

expect part2 testInput2 == "46"
expect
    seeds = [Unchanged {startsFrom: 8, endsBefore: 15}, Unchanged {startsFrom: 50, endsBefore: 56}]
    mapping ={ startFrom: 10, endBefore: 13, mapWithOffset: 100 } 
    mappedSeeds = applyMappingToRanges seeds mapping
    expectedMappedSeeds = [Changed {startsFrom: 110, endsBefore: 113}, Unchanged {startsFrom: 50, endsBefore: 56}, Unchanged {startsFrom: 8, endsBefore: 10}, Unchanged {startsFrom: 13, endsBefore: 15}]
    mappedSeeds == expectedMappedSeeds

expect
    seeds = [Unchanged {startsFrom: 8, endsBefore: 15}, Unchanged {startsFrom: 50, endsBefore: 56}]
    mappingGroup = [{ startFrom: 10, endBefore: 13, mapWithOffset: 100 }, { startFrom: 13, endBefore: 15, mapWithOffset: 200 }]
    mappedSeeds = applyMappingGroupToRanges seeds mappingGroup 
    expectedMappedSeeds = [ Unchanged {startsFrom: 110, endsBefore: 113}, Unchanged {startsFrom: 50, endsBefore: 56}, Unchanged {startsFrom: 8, endsBefore: 10}, Unchanged {startsFrom: 213, endsBefore: 215}]
    mappedSeeds == expectedMappedSeeds

applyMappingGroupsToRanges = \ranges, mappingGroups ->
    List.walk mappingGroups ranges applyMappingGroupToRanges 

applyMappingGroupToRanges = \ranges, mappingGroup ->
        groupMappedRanges = List.walk mappingGroup ranges applyMappingToRanges
        List.map groupMappedRanges \r ->
            when r is
                Changed a -> Unchanged a
                Unchanged _ -> r

applyMappingToRanges = \initRanges, mapping0  ->
    helper = \ranges, rangeIdx, mapping  ->
        when List.get ranges rangeIdx is
            Err _ -> ranges
            Ok (Changed _) -> helper ranges (rangeIdx + 1) mapping
            Ok (Unchanged {endsBefore, startsFrom}) ->
                {startFrom: mapStart, endBefore: mapEnd, mapWithOffset} = mapping
                outsideMappingRange = endsBefore <= mapStart || startsFrom >= mapEnd
                if outsideMappingRange then
                    helper ranges (rangeIdx + 1) mapping
                else
                    start = Num.max startsFrom mapStart
                    stop = Num.min endsBefore mapEnd
                    mappedRanges = List.set ranges rangeIdx (Changed {startsFrom: start + mapWithOffset, endsBefore: stop + mapWithOffset})

                    hasLowerUnchanged = startsFrom < mapStart
                    hasUpperUnchanged = endsBefore > mapEnd
                    if hasLowerUnchanged && hasUpperUnchanged then
                        newRangesPre = List.append mappedRanges (Unchanged {startsFrom: startsFrom, endsBefore: mapStart })
                        newRanges = List.append newRangesPre (Unchanged {startsFrom: mapEnd, endsBefore:endsBefore  })
                        helper newRanges (rangeIdx + 1) mapping
                    else if hasLowerUnchanged then
                        newRanges = List.append mappedRanges (Unchanged {startsFrom: startsFrom, endsBefore: mapStart })
                        helper newRanges (rangeIdx + 1) mapping
                    else if hasUpperUnchanged then
                        newRanges = List.append mappedRanges (Unchanged {startsFrom: mapEnd, endsBefore:endsBefore  })
                        helper newRanges (rangeIdx + 1) mapping
                    else
                        # mapping perfectly covers range
                        helper mappedRanges (rangeIdx + 1) mapping

    helper initRanges 0 mapping0

getStartOfRange = \l ->
    List.map l \r ->
        when r is
            Unchanged {startsFrom} -> startsFrom
            Changed {startsFrom} -> startsFrom

getForSure = \list, n ->
    when List.get list n is
        Ok elem -> elem
        Err _ ->
            crash "Out of bounds"

