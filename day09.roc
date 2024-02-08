app "AoC"
    packages {
        pf: "https://github.com/roc-lang/basic-cli/releases/download/0.7.0/bkGby8jb0tmZYsy2hg1E_B2QrCgcSTxdUlHtETwm5m4.tar.br",
    }
    imports [pf.Stdout, pf.Task.{ Task }, "input/day09.txt" as input : Str]
    provides [main] to pf

main : Task {} *
main =
    printAnswers (part1 input) (part2 input)

printAnswers = \p1, p2 ->
    Stdout.line "part1: $(p1)\npart2: $(p2)"

testInput1 = "1 3 6 10 15 21"
testInput2 = "10 13 16 21 30 45"
testInputFull = 
    """
    0 3 6 9 12 15
    1 3 6 10 15 21
    10 13 16 21 30 45
    """

expect 
    res = part1 testInput1
    exp = "28"
    res == exp

expect 
    res = part1 testInput2
    exp = "68"
    res == exp

parseInput = \in ->
    Str.split in "\n" |> List.map (\x -> Str.split x " " |> List.keepOks Str.toI64)

part1 = \in ->
    inp = parseInput in
    List.map inp predictNext |> List.sum |> Num.toStr

diffBetween = \l ->
    when l is
        [] -> []
        [a] -> []
        [.., a, b] -> 
            diffBetween (List.dropLast l 1) |> List.append (b - a)

predictNext = \initList ->
    predictHelp = \l, prediction->
        hasNonZero = List.any l \x -> x != 0 
        when l is
            [] -> prediction
            [.. as head, a] if hasNonZero -> predictHelp (diffBetween l) (prediction +a)
            _ -> prediction 

    predictHelp initList 0

predictPrev = \initList ->
    predictHelp = \l, prediction, sign->
        hasNonZero = List.any l \x -> x != 0 
        when l is
            [] -> prediction
            [a, .. as rest] if hasNonZero -> predictHelp (diffBetween l) (prediction + a * sign) (-1 * sign)
            _ -> prediction 

    predictHelp initList 0 1

part2 = \in ->
    inp = parseInput in
    List.map inp predictPrev |> List.sum |> Num.toStr

expect 
    res = part2 testInput2
    exp = "5"
    res == exp

expect 
    res = part2 testInputFull
    exp = "2"
    res == exp

#1762065988
#1066
