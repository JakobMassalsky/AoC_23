module Day07

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

let handRank (hand: int array): int =
    let jacks = hand |> Array.filter ((=) 7) |> Array.length
    let c = hand |> Array.groupBy id
    match c.Length with
        | 1 -> 6
        | 2 -> if jacks > 0 then 6 
                else if (snd c[0]).Length = 1 || (snd c[0]).Length = 4 then 5 else 4
        | 3 -> if (snd c[0]).Length = 2 || (snd c[1]).Length = 2 
                then match jacks with // twopair
                        | 2 -> 5 // upgrade to quads
                        | 1 -> 4 // upgrade to full house
                        | _ -> 2 // two pair
                else match jacks with // trips
                        | 0 -> 3 // trips
                        | _ -> 5 // upgrade to quads
        | 4 -> match jacks with // pair
                    | 2 -> 3 // upgrade to trips
                    | 1 -> 3 // upgrade to trips
                    | _ -> 1 // pair
        | _ -> if jacks > 0 then 1 else 0

let compHandTie (hand1: int array) (hand2: int array): int =
    let i = Array.init hand1.Length (fun i -> i) |> Array.find (fun i -> hand1[i] <> hand2[i])
    if hand1[i] < hand2[i] then -1 else 1

let compHand (hand1: int array) (hand2: int array): int =
    if hand1 = hand2 then 0 else
    let h1 = handRank hand1
    let h2 = handRank hand2
    if h1 <> h2 then if h1 < h2 then -1 else 1
    else compHandTie hand1 hand2

let getScore input = 
    input |> Array.sortWith (fun (h1, _) (h2, _) -> compHand h1 h2)
        |> Array.indexed
        |> Array.sumBy (fun (i, (_, b)) -> (i + 1) * Int32.Parse b)

let solvePart1 (input: string array array) =
    input |> Array.map (fun [|h: string ; b: string|] -> (h.ToCharArray() |> Array.map toCard, b))
        |> getScore

let solvePart2 (input) =
    input |> Array.map (fun [|h: string ; b: string|] -> (h.Replace('J', char(7)).ToCharArray() |> Array.map toCard, b))
        |> getScore

let solve =
    let input = readInput "day07.txt"
                |> Array.map (stringSplit ' ' StringSplitOptions.RemoveEmptyEntries)

    // Solve Part 1
    let resultPart1 = solvePart1 input
    printfn "Part 1 Result: %A" resultPart1

    // Solve Part 2
    let resultPart2 = solvePart2 input
    printfn "Part 2 Result: %A" resultPart2

