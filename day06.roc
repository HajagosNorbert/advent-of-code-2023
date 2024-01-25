app "AoC"
    packages {
        pf: "https://github.com/roc-lang/basic-cli/releases/download/0.7.0/bkGby8jb0tmZYsy2hg1E_B2QrCgcSTxdUlHtETwm5m4.tar.br",
    }
    imports [pf.Stdout, pf.Task.{ Task }, "input/day06.txt" as input : Str]
    provides [main] to pf

main : Task {} *
main =
    printAnswers (part1 input) (part2 input)

printAnswers = \p1, p2 ->
    Stdout.line "part1: $(p1)\npart2: $(p2)"

parseInputPart1 = \in ->
    structuredInput = Str.split in "\n" |> List.map \line -> Str.split line " " |> List.keepOks Str.toI64
    List.map2 (getForSure structuredInput 0) (getForSure structuredInput 1) \time, dist ->
        {time, dist}

testInput1 =
    """
    Time:      7  15   30
    Distance:  9  40  200
    """

part1 = \in ->
    inp = parseInputPart1 in
    waysToWinList = List.map inp waysToWin
    res = List.product waysToWinList
    Num.toStr res
    
parseInputPart2 = \in ->
    structuredInput = Str.split in "\n" |> List.map \line -> Str.split line " " |> List.dropFirst 1 |> List.dropIf \str -> str == ""
    [time, dist] = List.map structuredInput (\nums -> List.walk nums "" Str.concat) |> List.keepOks Str.toI64
    {time, dist}

waysToWin = \{time, dist} ->
    duplicatedWins = (time + 1) % 2 
    midpoint = time // 2
    winsBelowMidpoint = validCombinations midpoint (time - midpoint) dist 0
    2 * winsBelowMidpoint - duplicatedWins
    
validCombinations = \buttonTime, travellTime, record, found ->
    dist = buttonTime * travellTime
    if dist <= record then
        found
    else
        validCombinations (buttonTime-1) (travellTime+1) record (found+1)

expect part1 testInput1 == "288"

testInput2 = testInput1

part2 = \in ->
    inp = parseInputPart2 in
    waysToWin inp |> Num.toStr

getForSure = \list, n ->
    when List.get list n is
        Ok elem -> elem
        Err _ ->
            crash "Out of bounds"
