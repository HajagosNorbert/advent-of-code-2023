app "AoC"
    packages {
        pf: "https://github.com/roc-lang/basic-cli/releases/download/0.7.0/bkGby8jb0tmZYsy2hg1E_B2QrCgcSTxdUlHtETwm5m4.tar.br",
    }
    imports [pf.Stdout, pf.Task.{ Task }, "input/day08.txt" as input : Str]
    provides [main] to pf

main : Task {} *
main =
    printAnswers (part1 input) (part2 input)

printAnswers = \p1, p2 ->
    Stdout.line "part1: $(p1)\npart2: $(p2)"

testInput1 =
    """
    LLR

    AAA = (BBB, BBB)
    BBB = (AAA, ZZZ)
    ZZZ = (ZZZ, ZZZ)
    """

testInput2 =
    """
    LR

    11A = (11B, XXX)
    11B = (XXX, 11Z)
    11Z = (11B, XXX)
    22A = (22B, XXX)
    22B = (22C, 22C)
    22C = (22Z, 22Z)
    22Z = (22B, 22B)
    XXX = (XXX, XXX)
    """

part1 = \in ->
    {dirs, nodes} = parseInput in
    escapeDesert { dirs, nodes, startNode: "AAA"} (\node -> node == "ZZZ") |> Num.toStr

parseInput = \in ->
    inp = Str.split in "\n\n"
    dirs = List.get inp 0 |> unwrap |> Str.toUtf8
    nodes =
        List.get inp 1
        |> unwrap
        |> Str.split "\n"
        |> List.map Str.toUtf8
        |> List.map
            (\line ->
                (
                    Str.fromUtf8Range line { start: 0, count: 3 } |> unwrap,
                    {
                        left: Str.fromUtf8Range line { start: 7, count: 3 } |> unwrap,
                        right: Str.fromUtf8Range line { start: 12, count: 3 } |> unwrap,
                    }
                ))
        |> Dict.fromList
    {dirs, nodes}

escapeDesert = \{ dirs: dirs0, nodes: nodes0, startNode}, predicate ->
    escapeDesertHelp {dirs: dirs0, nodes:nodes0, dirIdx: 0, node: startNode, stepCount: 0} predicate

escapeDesertHelp = \{ dirs, dirIdx, nodes, node, stepCount}, predicate ->
    if predicate node  then
        stepCount
    else
        wrappingDirIdx = dirIdx % List.len dirs 
        currDir = List.get dirs wrappingDirIdx |> unwrap
        paths = Dict.get nodes node |> unwrap
        nextNode = when currDir is 
            'L' -> paths.left
            'R'-> paths.right
            _ -> crash "invalid direction in input"
        escapeDesertHelp {dirs, dirIdx: wrappingDirIdx + 1, nodes, node: nextNode, stepCount: stepCount + 1} predicate


expect
    part1 testInput1 == "6"

part2 = \in ->
    {dirs, nodes} = parseInput in
    startNodes = Dict.keys nodes |> List.keepIf \node -> Str.endsWith node "A"
    steps = List.map startNodes \startNode ->
        escapeDesert {dirs, nodes, startNode: startNode} \x -> Str.endsWith x "Z"
    List.walk steps 1 lcd |> Num.toStr


## Greatest Common Divisor with the euclidean algorithm
gcd = \a, b ->
    rem = a % b
    if rem == 0 then
        b
    else
        gcd b rem 

## Least Common Denominator
lcd = \ a, b ->
    (a // (gcd a b)) * b


unwrap = \x ->
    when x is
        Ok elem -> elem
        Err _ -> 
            #dbg x
            crash "Unwraped an error!"
