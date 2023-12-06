module Day06

open Utils
open System

let calcRaceMargin (time: int64) (minDist: int64) =
    Array.init (int(time) + 1) (fun i -> (time - int64 (i)) * int64 (i))
        |> Array.filter ((<) minDist) 
        |> Array.length

let solvePart1 (input: int64 array array) =
    (input[0], input[1]) ||> Array.map2 calcRaceMargin |> Array.reduce (*)

let solve =
    let input = readInput "day06.txt" 
        
    let p1Input = input |> Array.map ((stringSplit ' ' StringSplitOptions.RemoveEmptyEntries)
            >> Array.skip 1 >> Array.map Int64.Parse)
    let resultPart1 = solvePart1 p1Input
    printfn "Part 1 Result: %A" resultPart1

    let p2Input = input |> Array.map (fun s -> (s.Substring(9).Replace(" ", ""))
                >> Int64.Parse)
    let resultPart2 = calcRaceMargin p2Input[0] p2Input[1]
    printfn "Part 2 Result: %A" resultPart2

