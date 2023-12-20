app "AoC"
    packages {
        pf: "https://github.com/roc-lang/basic-cli/releases/download/0.7.0/bkGby8jb0tmZYsy2hg1E_B2QrCgcSTxdUlHtETwm5m4.tar.br",
    }
    imports [pf.Stdout, pf.Task.{ Task }, "input/day04.txt" as input : Str]
    provides [main] to pf

testInput1 =
    """
    Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53
    Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19
    Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1
    Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83
    Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36
    Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11
    """

main : Task {} *
main =
    printAnswers (part1 input) (part2 input)

printAnswers = \p1, p2 ->
    Stdout.line "part1: \(p1)\npart2: \(p2)"

part1 = \in ->
    winnerCardCounts = getWinnerCardCounts in
    cardPoints = List.map winnerCardCounts \count ->
        if count == 0 then
            0
        else
            Num.shiftLeftBy 1 (Num.toU8 (count - 1))
    pointsSum = List.sum cardPoints
    pointsSum |> Num.toStr

expect part1 testInput1 == "13"

testInput2 = testInput1

part2 = \in ->
    winnerCardCounts = getWinnerCardCounts in
    initialCardAmounts = List.map winnerCardCounts \count -> { amount: 1, won: count }
    cardsWithAmountAndWonInfo = scratchCards initialCardAmounts 0

    cardAmounts = List.map cardsWithAmountAndWonInfo .amount
    totalCards = List.sum cardAmounts
    totalCards |> Num.toStr

expect part2 testInput2 == "30"

getWinnerCardCounts = \in ->
    rawGames =
        Str.split in "\n"
        |> List.map \line -> Str.split line ": "
            |>
            getForSure 1
            |> Str.split "|"
            |> List.map \winnersAndCards -> Str.split winnersAndCards " " |> List.keepOks Str.toU8
    winnersAndCardsSet = List.map rawGames \winnersAndCards -> List.map winnersAndCards Set.fromList
    winnerCards = List.map winnersAndCardsSet \wAndC ->
        winners = getForSure wAndC 0
        cards = getForSure wAndC 1
        Set.intersection winners cards

    winnerCardCounts = List.map winnerCards Set.len
    winnerCardCounts

getForSure = \list, n ->
    when List.get list n is
        Ok elem -> elem
        Err _ ->
            crash "Out of bounds"

scratchCards = \cards, i ->
    when List.get cards i is
        Err OutOfBounds -> cards
        Ok { amount, won } ->
            newCards = addWonCards cards (i + 1) { amount, cardsBelowToAddTo: won }
            scratchCards newCards (i + 1)

addWonCards = \cards, currCardIdx, { amount, cardsBelowToAddTo } ->
    if cardsBelowToAddTo == 0 then
        cards
    else
        card = getForSure cards currCardIdx
        newCards = List.set cards currCardIdx { amount: card.amount + amount, won: card.won }
        addWonCards newCards (currCardIdx + 1) { amount, cardsBelowToAddTo: cardsBelowToAddTo - 1 }


