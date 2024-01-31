app "AoC"
    packages {
        pf: "https://github.com/roc-lang/basic-cli/releases/download/0.7.0/bkGby8jb0tmZYsy2hg1E_B2QrCgcSTxdUlHtETwm5m4.tar.br",
    }
    imports [pf.Stdout, pf.Task.{ Task }, "input/day08.txt" as input : Str]
    provides [main] to pf

main : Task {} *
main =
    #printAnswers (part1 testInput1) (part2 testInput2)
    printAnswers "HI" (part2 input)

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
    dirsAndNodes = parseInput in
    escapeDesert dirsAndNodes (\node -> node == ['Z', 'Z', 'Z']) |> Num.toStr

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
                    List.sublist line { start: 0, len: 3 },
                    {
                        left: List.sublist line { start: 7, len: 3 },
                        right: List.sublist line { start: 12, len: 3 },
                    },
                ))
        |> Dict.fromList
    {dirs, nodes}

escapeDesert = \{ dirs: dirs0, nodes: nodes0 }, predicate ->
    escapeDesertHelp {dirs: dirs0, nodes:nodes0, dirIdx: 0, node: ['A', 'A', 'A'], stepCount: 0} predicate

escapeDesertHelp = \{ dirs, dirIdx, nodes, node, stepCount}, predicate ->
    if predicate node  then
        stepCount
    else
        wrappingDirIdx = dirIdx % List.len dirs 
        currDir = List.get dirs wrappingDirIdx |> unwrap
        dbg T node
        paths = Dict.get nodes node |> unwrap
        nextNode = when currDir is 
            'L' -> paths.left
            'R'-> paths.right
            _ -> crash "invalid direction in input"
        escapeDesertHelp {dirs, dirIdx: wrappingDirIdx + 1, nodes, node: nextNode, stepCount: stepCount + 1} predicate


expect
    part1 testInput1 == "6"

part2 = \in ->
    dirsAndNodes = parseInput in
    escapeDesertGhost dirsAndNodes |> Num.toStr

escapeDesertGhost = \{ dirs, nodes} ->
    startNodes = Dict.keys nodes |> List.keepIf \node -> 
        a = List.last node 
        dbg a
        a |> unwrap == 'A'
    escapeDesertHelpGhost {dirs, nodes, dirIdx: 0, nodesWalking: startNodes, stepCount: 0} 

escapeDesertHelpGhost = \{ dirs, dirIdx, nodes, nodesWalking, stepCount} ->
    if List.all nodesWalking (\node -> List.last node |> unwrap == 'Z') then
        stepCount
    else
        wrappingDirIdx = dirIdx % List.len dirs 
        currDir = List.get dirs wrappingDirIdx |> unwrap
        walkOptions = List.map nodesWalking \n->
            Dict.get nodes n |> unwrap
        nextNodesWalking = when currDir is 
            'L' -> List.map walkOptions .left
            'R'-> List.map walkOptions .right
            _ -> crash "invalid direction in input"
        escapeDesertHelpGhost {dirs, dirIdx: wrappingDirIdx + 1, nodes, nodesWalking: nextNodesWalking, stepCount: stepCount + 1}



unwrap = \x ->
    when x is
        Ok elem -> elem
        Err _ -> 
            dbg x
            crash "Unwraped an error!"
