app "AoC"
    packages {
        pf: "https://github.com/roc-lang/basic-cli/releases/download/0.7.0/bkGby8jb0tmZYsy2hg1E_B2QrCgcSTxdUlHtETwm5m4.tar.br",
    }
    imports [pf.Stdout, pf.Task.{ Task }, "input/day03.txt" as input : Str]
    provides [main] to pf

main : Task {} *
main =
    printAnswers (part1 input) (part2 input)

printAnswers = \p1, p2 ->
    Stdout.line "part1: \(p1)\npart2: \(p2)"

testInput1 =
    """
    467..114..
    ...*......
    ..35..633.
    ......#...
    617*......
    .....+.58.
    ..592.....
    ......755.
    ...$.*....
    .664.598..
    """
part1 = \inpt ->
    lines = Str.split inpt "\n" |> List.map Str.toUtf8
    firstLine =
        when List.first lines is
            Err _ -> crash "no text provided"
            Ok l -> l
    width = List.len firstLine

    taggedLines = List.map lines \line ->
        List.map line \char ->
            if char == '.' then
                Empty
            else if isNumeric char then
                InactiveDigit char
            else
                Symbol char

    lWithActiveDigits = activateDigitsAdjecentToSymbols taggedLines width

    rowSums = List.map lWithActiveDigits \l ->
        feedNumIntoSum = \{ num, status, rowSum } ->
            valueOfPrevNum =
                when status is
                    Inactive -> 0
                    Active -> num
            { num: 0, status: Inactive, rowSum: rowSum + valueOfPrevNum }
        rowState = List.walk l ({ rowSum: 0, num: 0, status: Inactive }) \s, tile ->
            when tile is
                ActiveDigit char -> { num: s.num * 10 + (toDigit char), status: Active, rowSum: s.rowSum }
                InactiveDigit char -> { s & num: s.num * 10 + (toDigit char) }
                Symbol _ | Empty -> feedNumIntoSum s
        finalState = feedNumIntoSum rowState
        finalState.rowSum
    List.sum rowSums |> Num.toStr

expect part1 testInput1 == "4361"

activateDigitsAdjecentToSymbols = \lines, width ->
    help = \l, y, x ->
        curr = List.get l y |> Result.try \row -> List.get row x
        when curr is
            Err _ -> l
            Ok currTile ->
                (nextY, nextX) =
                    if x < width - 1 then
                        (y, x + 1)
                    else
                        (y + 1, 0)

                currRow =
                    when List.get l y is
                        Ok a -> a
                        Err _ -> crash "impossible"

                adjecents =
                    getOne = \yy, xx ->
                        List.get l yy |> Result.try \row -> List.get row xx |> Result.try \elem -> Ok (elem, yy, xx)
                    e = getOne y (x + 1)
                    s = getOne (y + 1) (x)
                    se = getOne (y + 1) (x + 1)
                    sw =
                        if x == 0 then
                            Err OutOfBounds
                        else
                            getOne (y + 1) (x - 1)
                    List.keepOks [e, se, s, sw] id

                updateAdjecents = \_ ->
                    li, adj <- List.walk adjecents l
                    res =
                        when adj is
                            (InactiveDigit d, yy, xx) ->
                                yRow =
                                    when List.get li yy is
                                        Err _ -> crash "impossible"
                                        Ok row -> row
                                List.set li yy (List.set yRow xx (ActiveDigit d))

                            _ -> li
                    res

                updateCurrent = \digit ->
                    hasSymbolAdjecent = List.any adjecents \c ->
                        when c is
                            (Symbol _, _, _) -> Bool.true
                            _ -> Bool.false
                    if hasSymbolAdjecent then
                        List.set l y (List.set currRow x (ActiveDigit digit))
                    else
                        l

                when currTile is
                    Symbol _ ->
                        newLines = updateAdjecents 1
                        help newLines nextY nextX

                    InactiveDigit d ->
                        newLines = updateCurrent d
                        help newLines nextY nextX

                    ActiveDigit _ ->
                        help l nextY nextX

                    Empty ->
                        help l nextY nextX

    help lines 0 0

id = \a -> a

testInput2 =
    """
    467..114..
    ...*......
    ..35..633.
    ......#...
    617*......
    .....+.58.
    ..592.....
    ......755.
    ...$.*....
    .664.598..
    """

part2 = \inpt ->
    lines = Str.split inpt "\n" |> List.map Str.toUtf8

    ratioSum = calcRatiosSum lines

    ratioSum |> Num.toStr

expect part2 testInput2 == "467835"

isNumeric = \char -> '0' <= char && char <= '9'
toDigit = \char -> Num.toU64 (char - '0')

calcRatiosSum = \lines ->
    List.walkWithIndex lines 0 \ratioSum, line, y ->
        List.walkWithIndex line ratioSum \sum, c, x ->
            when c is
                '*' -> sum + ratioOfGear lines y x
                _ -> sum

ratioOfGear = \lines, y, x ->
    lineOfGear = getForSure lines y
    north =
        if y == 0 then
            None
        else
            rowAbove = getForSure lines (y - 1)
            linePartNumbers rowAbove x
    south =
        rowBelow = List.get lines (y + 1)
        when rowBelow is
            Err _ -> None
            Ok row -> linePartNumbers row x
    west = getNum lineOfGear Before x |> mapResultToPartNumberAmount
    east = getNum lineOfGear After x |> mapResultToPartNumberAmount

    partNumber = List.walkUntil [north, south, west, east] None \partNums, dir ->
        when (partNums, dir) is
            (None, None) -> Continue None
            (None, One _ as single) -> Continue single
            (None, Two _ _ as pair) -> Break pair
            (One _ as single, None) -> Continue single
            (One a, One b) -> Break (Two a b)
            (Two _ _ as pair, None) -> Break pair
            (One _, Two _ _) | (Two _ _, One _) | (Two _ _, Two _ _) -> crash "more than 2 part numbers found!"

    when partNumber is
        Two a b -> a * b
        _ -> 0

mapResultToPartNumberAmount = \res ->
    when res is
        Ok num -> One num
        Err _ -> None

linePartNumbers = \line, x ->
    middle = getNum line Enclosing x
    when middle is
        Ok num -> One num
        Err _ ->
            left = getNum line Before x
            right = getNum line After x
            when (left, right) is
                (Ok a, Ok b) -> Two a b
                (Ok a, Err _) -> One a
                (Err _, Ok b) -> One b
                (Err _, Err _) -> None

getNum = \line, loc, x ->
    calcNumBeforeX = \state ->
        charsBeforeX = List.sublist line { start: 0, len: x }
        List.walkBackwardsUntil charsBeforeX state \{ sum, digitCount }, c ->
            if isNumeric c then
                newDigit = toDigit c
                newSum = newDigit * (Num.powInt 10 digitCount) + sum
                Continue { sum: newSum, digitCount: digitCount + 1 }
            else
                Break { sum, digitCount }
    calcNumAfterX = \state ->
        charsAfterX = List.sublist line { start: x + 1, len: List.len line }
        List.walkUntil charsAfterX state \{ sum, digitCount }, c ->
            if isNumeric c then
                newDigit = toDigit c
                newSum = 10 * sum + newDigit
                Continue { sum: newSum, digitCount: digitCount + 1 }
            else
                Break { sum, digitCount }

    resultify = \state ->
        if state.digitCount == 0 then
            Err InvalidNumStr
        else
            Ok state.sum

    when loc is
        Before -> { sum: 0, digitCount: 0 } |> calcNumBeforeX |> resultify
        After -> { sum: 0, digitCount: 0 } |> calcNumAfterX |> resultify
        Enclosing ->
            charAtX = getForSure line x
            if isNumeric charAtX then
                { sum: toDigit charAtX, digitCount: 1 } |> calcNumBeforeX |> calcNumAfterX |> \num -> Ok num.sum
            else
                Err InvalidNumStr

getForSure = \list, n ->
    when List.get list n is
        Ok a -> a
        Err _ -> crash "out of bounds"

