app "AoC"
    packages {
        pf: "https://github.com/roc-lang/basic-cli/releases/download/0.7.0/bkGby8jb0tmZYsy2hg1E_B2QrCgcSTxdUlHtETwm5m4.tar.br",
    }
    imports [pf.Stdout, pf.Task.{ Task }, "input/day07.txt" as input : Str]
    provides [main] to pf

main : Task {} *
main =
    printAnswers (part1 input) (part2 input)

printAnswers = \p1, p2 ->
    Stdout.line "part1: $(p1)\npart2: $(p2)"

testInput1 =
    """
    32T3K 765
    T55J5 684
    KK677 28
    KTJJT 220
    QQQJA 483
    """

part1 = \in ->
    inp = parseInput in
    hands = List.map inp analyzeCards
    sortedHands = List.sortWith hands cmpHands
    calcWinnings sortedHands

calcWinnings  = \ hands ->
    List.mapWithIndex hands (\{ bid }, i -> (Num.toU64 (i + 1)) * bid) |> List.sum |> Num.toStr
    
cmpHands = \{ power: power1, cards: cards1 }, { power: power2, cards: cards2 } ->
    powCmp = Num.compare (toNumericPower power1) (toNumericPower power2)
    when powCmp is
        EQ -> cmpCards cards1 cards2
        a -> a

cmpCards = \cards1, cards2 ->
    when (cards1, cards2) is
        ([], []) -> EQ
        ([card1, .. as rest1], [card2, .. as rest2]) ->
            when Num.compare card1 card2 is
                EQ -> cmpCards rest1 rest2
                a -> a

        _ -> crash "Impossible"

expect
    part1 testInput1 == "6440"

expect
    part2 testInput2 == "5905"

parseInput = \in ->
    hands =
        Str.split in "\n"
        |> List.map \line -> Str.split line " "
            |> \l ->
                when l is
                    [cardsTxt, bid] ->
                        { cardsTxt: Str.toUtf8 cardsTxt, bid: Str.toU64 bid |> Result.withDefault 0 }

                    _ -> crash "incorrect format"
    hands

analyzeCards = \{cardsTxt, bid} ->
    numCards = List.map cardsTxt toNumericCard
    power = determinPower numCards
    { power, cards: numCards, bid }

analyzeCardsWithJoker = \{cardsTxt, bid} ->
    numCards = List.map cardsTxt toNumericCardWithJoker
    power = determinPowerWithJoker numCards
    { power, cards: numCards, bid }

expect
    cardsTxt = "23456" |> Str.toUtf8
    {power} = analyzeCardsWithJoker {cardsTxt, bid:1}
    power == High

expect
    cardsTxt = "2345J" |> Str.toUtf8
    {power} = analyzeCardsWithJoker {cardsTxt, bid:1}
    power == One

expect
    cardsTxt = "234JJ" |> Str.toUtf8
    {power} = analyzeCardsWithJoker {cardsTxt, bid:1}
    power == Three

expect
    cardsTxt = "23JJJ" |> Str.toUtf8
    {power} = analyzeCardsWithJoker {cardsTxt, bid:1}
    power == Four

expect
    cardsTxt = "2JJJJ" |> Str.toUtf8
    {power} = analyzeCardsWithJoker {cardsTxt, bid:1}
    power == Five

expect
    cardsTxt = "JJJJJ" |> Str.toUtf8
    {power} = analyzeCardsWithJoker {cardsTxt, bid:1}
    power == Five

expect
    cardsTxt = "2233J" |> Str.toUtf8
    {power} = analyzeCardsWithJoker {cardsTxt, bid:1}
    power == Full

expect
    cardsTxt = "2234J" |> Str.toUtf8
    {power} = analyzeCardsWithJoker {cardsTxt, bid:1}
    power == Three


toNumericPower = \pow ->
    when pow is
        Five -> 6
        Four -> 5
        Full -> 4
        Three -> 3
        Two -> 2
        One -> 1
        High -> 0

toNumericCard = \card ->
    when card is
        'A' -> 12
        'K' -> 11
        'Q' -> 10
        'J' -> 9
        'T' -> 8
        '9' -> 7
        '8' -> 6
        '7' -> 5
        '6' -> 4
        '5' -> 3
        '4' -> 2
        '3' -> 1
        '2' -> 0
        _ -> crash "invalid card"


toNumericCardWithJoker = \card ->
    when card is
        'A' -> 12
        'K' -> 11
        'Q' -> 10
        'T' -> 9
        '9' -> 8
        '8' -> 7
        '7' -> 6
        '6' -> 5
        '5' -> 4
        '4' -> 3
        '3' -> 2
        '2' -> 1
        'J' -> 0
        _ -> crash "invalid card"

determinPower = \hand ->
    occurences = List.walk hand (List.repeat 0 13) \acc, card ->
        currCount = getForSure acc card
        List.set acc card (currCount + 1)
    mostCommon = List.max occurences |> Result.withDefault 0
    mostCommonIdx = List.findFirstIndex occurences (\x -> x == mostCommon) |> Result.withDefault 0

    occurencesWithoutMostCommon = List.set occurences mostCommonIdx 0
    secondMostCommon = List.max occurencesWithoutMostCommon |> Result.withDefault 0
    powerOfTop2Card mostCommon secondMostCommon

determinPowerWithJoker = \hand ->
    occurences = List.walk hand (List.repeat 0 13) \acc, card ->
        currCount = getForSure acc card
        List.set acc card (currCount + 1)
    mostCommon = List.max occurences |> Result.withDefault 0
    mostCommonIdx = List.findFirstIndex occurences (\x -> x == mostCommon) |> Result.withDefault 0

    occurencesWithoutMostCommon = List.set occurences mostCommonIdx 0
    secondMostCommon = List.max occurencesWithoutMostCommon |> Result.withDefault 0
    secondMostCommonIdx = List.findFirstIndex occurencesWithoutMostCommon (\x -> x == secondMostCommon) |> Result.withDefault 0

    occurencesWithoutSecMostCommon = List.set occurencesWithoutMostCommon secondMostCommonIdx 0
    thirdMostCommon = List.max occurencesWithoutSecMostCommon |> Result.withDefault 0
    
    (top1, top2) = 
        if mostCommonIdx == 0 || secondMostCommonIdx == 0 then
            (secondMostCommon + mostCommon, thirdMostCommon)
        else
            jokers = getForSure occurencesWithoutSecMostCommon 0
            (mostCommon + jokers, secondMostCommon)

    powerOfTop2Card top1 top2

powerOfTop2Card = \mostCommon,secondMostCommon ->
    when (mostCommon, secondMostCommon) is
        (5, _) -> Five
        (4, _) -> Four
        (3, 2) -> Full
        (3, _) -> Three
        (2, 2) -> Two
        (2, _) -> One
        (1, _) -> High
        _ -> crash "Impossible occurence count"


testInput2 = testInput1

part2 = \in ->
    inp = parseInput in
    hands = List.map inp analyzeCardsWithJoker
    sortedHands = List.sortWith hands cmpHands
    calcWinnings sortedHands

getForSure = \list, n ->
    when List.get list n is
        Ok elem -> elem
        Err _ ->
            crash "Out of bounds"
