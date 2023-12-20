app "AoC"
    packages {
        pf: "https://github.com/roc-lang/basic-cli/releases/download/0.7.0/bkGby8jb0tmZYsy2hg1E_B2QrCgcSTxdUlHtETwm5m4.tar.br",
    }
    imports [pf.Stdout, pf.Task.{ Task }, "input/day01.txt" as input : Str]
    provides [main] to pf

main : Task {} *
main =
    printAnswers (part1 input) (part2 input)

printAnswers = \p1, p2 ->
    Stdout.line "part1: \(p1)\npart2: \(p2)"

testInput1 =
    """
    1abc2
    pqr3stu8vwx
    a1b2c3d4e5f
    treb7uchet
    """
part1 = \in ->
    lines = Str.split in "\n" |> List.map Str.toUtf8
    firstNumChars = List.keepOks lines \line ->
        List.findFirst line isNumeric

    lastNumChars = List.keepOks lines \line ->
        List.findLast line isNumeric

    firstNums = List.map firstNumChars toDigit
    lastNums = List.map lastNumChars toDigit

    lineNums = List.map2 firstNums lastNums numFrom2Digit

    List.sum lineNums |> Num.toStr

testInput2 =
    """
    two1nine
    eightwothree
    abcone2threexyz
    xtwone3four
    4nineeightseven2
    zoneight234
    7pqrstsixteen
    """
part2 = \in ->
    lineValues = Str.split in "\n" |> List.map Str.toUtf8 |> List.map lineValue
    List.keepOks lineValues id
    |> List.map
        (\(d1, d2) ->
            numFrom2Digit d1 d2)
    |> List.sum
    |> Num.toStr

lineValue = \line ->
    found = \digit, state ->
        when state is
            Err _ -> Ok (digit, digit)
            Ok (first, last) -> Ok (first, digit)

    help = \l, state ->
        when l is
            [] -> state
            [x, ..] if isNumeric x -> help (List.dropFirst l 1) (found (toDigit x) state)
            ['o', 'n', 'e', ..] -> help (List.dropFirst l 1) (found 1 state)
            ['t', 'w', 'o', ..] -> help (List.dropFirst l 1) (found 2 state)
            ['t', 'h', 'r', 'e', 'e', ..] -> help (List.dropFirst l 1) (found 3 state)
            ['f', 'o', 'u', 'r', ..] -> help (List.dropFirst l 1) (found 4 state)
            ['f', 'i', 'v', 'e', ..] -> help (List.dropFirst l 1) (found 5 state)
            ['s', 'i', 'x', ..] -> help (List.dropFirst l 1) (found 6 state)
            ['s', 'e', 'v', 'e', 'n', ..] -> help (List.dropFirst l 1) (found 7 state)
            ['e', 'i', 'g', 'h', 't', ..] -> help (List.dropFirst l 1) (found 8 state)
            ['n', 'i', 'n', 'e', ..] -> help (List.dropFirst l 1) (found 9 state)
            _ -> help (List.dropFirst l 1) state

    help line (Err NotFound)

id = \a -> a
isNumeric = \char -> '0' <= char && char <= '9'
toDigit = \char -> char - '0'
numFrom2Digit = \d1, d2 ->
    Num.toU64 (10 * d1 + d2)
