module Day07

#nowarn "0025"
open Utils
open System

let toCard (c: Char) =
    match c with
        | 'A' -> 104
        | 'K' -> 103
        | 'Q' -> 102
        | 'J' -> 101
        | 'T' -> 100
        | _ -> int(c)

let handRank (hand: int array): int array =
    let jacks = hand |> Array.filter ((=) 7) |> Array.length
    let h = hand |> Array.countBy id |> Array.filter (fst >> (<>) 7) |> Array.map snd |> Array.sortDescending
    match h with
        | [||] -> [|jacks|]
        | _ -> Array.updateAt 0 (h[0] + jacks) h

let compHand (hand1: int array, _) (hand2: int array, _): int =
    if hand1 = hand2 then 0 else
    let (hr1, hr2) = (hand1 |> handRank, hand2 |> handRank)
    let minL = min hr1.Length hr2.Length
    let c = compare (hr1 |> Array.take minL) (hr2 |> Array.take minL)
    if c <> 0 then c else compare hand1 hand2

let getScore input = 
    input |> Array.sortWith compHand
        |> Array.indexed
        |> Array.sumBy (fun (i, (_, b)) -> (i + 1) * Int32.Parse b)

let solvePart1 (input: string array) =
    input |> Array.map (stringSplit ' ' StringSplitOptions.RemoveEmptyEntries)
        |> Array.map (fun [|h: string ; b: string|] -> (h.ToCharArray() |> Array.map toCard, b))
        |> getScore

let solve =
    let input = readInput "day07.txt"

    // Solve Part 1
    let resultPart1 = solvePart1 input
    printfn "Part 1 Result: %A" resultPart1

    // Solve Part 2
    let resultPart2 = input |> Array.map (fun s -> s.Replace('J', char(7))) |> solvePart1
    printfn "Part 2 Result: %A" resultPart2


// Artifacts of ancient times

    // match c |> Array.map fst  with
    //     | [|5|] -> 6
    // match c.Length with
    //     | 1 -> 6
    //     | 2 -> if jacks > 0 then 6 
    //             else if (snd c[0]) = 1 || (snd c[0]) = 4 then 5 else 4
    //     | 3 -> if (snd c[0]) = 2 || (snd c[1]) = 2 
    //             then match jacks with // twopair
    //                     | 2 -> 5 // upgrade to quads
    //                     | 1 -> 4 // upgrade to full house
    //                     | _ -> 2 // two pair
    //             else match jacks with // trips
    //                     | 0 -> 3 // trips
    //                     | _ -> 5 // upgrade to quads
    //     | 4 -> match jacks with // pair
    //                 | 2 -> 3 // upgrade to trips
    //                 | 1 -> 3 // upgrade to trips
    //                 | _ -> 1 // pair
    //     | _ -> if jacks > 0 then 1 else 0
