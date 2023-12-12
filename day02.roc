app "AoC"
    packages {
        pf: "https://github.com/roc-lang/basic-cli/releases/download/0.7.0/bkGby8jb0tmZYsy2hg1E_B2QrCgcSTxdUlHtETwm5m4.tar.br",
    }
    imports [pf.Stdout, pf.Task.{Task}, "input/day02.txt" as input : Str]
    provides [main] to pf

testInput1 = 
"""
Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue
Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red
Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red
Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green
"""

main : Task {} *
main = 
    printAnswers (part1 input) (part2 input)

printAnswers = \ p1, p2 ->
    Stdout.line "part1: \(p1)\npart2: \(p2)"

part1 = \in ->
    maxRed= 12
    maxGreen = 13
    maxBlue = 14
    games = toPairSets in

    gameIdSum = 
        List.walkWithIndex games 0 \sum, sets, idx ->
            invalidGame = List.any sets \ colorSet ->
                List.any colorSet \ pair ->
                    when pair is
                        (x, "red") -> x > maxRed
                        (x, "green") -> x > maxGreen
                        (x, "blue") -> x > maxBlue
                        _ -> crash "not rgb color"

            if invalidGame then
                sum
            else
                gameId = idx + 1
                sum + gameId
    Num.toStr gameIdSum

toPairSets = \in ->
    sets <- Str.split in "\n" |> List.map (\game -> Str.split game ":") |> List.keepOks getIndex1 |> List.map 
    colorSet <- Str.split sets ";" |> List.map 
    countColorPair <- Str.split colorSet "," |> List.map Str.trim |> List.map 
    countAndColor = Str.split countColorPair " " 
    when countAndColor is 
        [count, color] -> 
            when Str.toU64 count is
                Ok num -> (num, color)
                Err _ -> crash "Not a number"
        _ -> crash "Not a count - color pair"

getIndex1 = \l -> List.get l 1 

expect part1 testInput1 == "8"

testInput2 = 
"""
Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue
Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red
Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red
Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green
"""

part2 = \inpt ->
    games = toPairs inpt
    mins = (0, 0, 0)
    powers = 
        pairs <- List.map games
        minColors = 
            currMins, pair <- List.walk pairs mins
            when pair is
                (x, "red") if x > currMins.0 -> (x, currMins.1, currMins.2)
                (x, "green")if x > currMins.1 -> (currMins.0, x, currMins.2) 
                (x, "blue")if x > currMins.2 -> (currMins.0, currMins.1, x) 
                _ -> currMins
        minColors.0 * minColors.1 * minColors.2

    gameIdSum = List.sum powers
    Num.toStr gameIdSum

toPairs = \in ->
    sets <- Str.split in "\n" |> List.map (\game -> Str.split game ":") |> List.keepOks getIndex1 |> List.map 
    colorSets = 
        colorSet <- Str.split sets ";" |> List.map 
        countColorPair <- Str.split colorSet "," |> List.map Str.trim |> List.map 
        countAndColor = Str.split countColorPair " " 
        when countAndColor is 
            [count, color] -> 
                when Str.toU64 count is
                    Ok num -> (num, color)
                    Err _ -> crash "Not a number"
            _ -> crash "Not a count - color pair"
    List.join colorSets


expect part2 testInput2 == "2286"
