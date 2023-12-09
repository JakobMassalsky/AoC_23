module Day09

open Utils
open System

let convergeSequence (seqqq: int array) =
    let rec reductions (v: int array) = seq { 
        yield v
        yield! (reductions (v |> Array.pairwise |> Array.map ((fun (a, b) -> b-a))))
    }
    seqqq |> reductions 
        |> Seq.takeWhile (Array.exists ((<>) 0))

let solvePart1 (input) =
    convergeSequence input |> Seq.sumBy Array.last

let solvePart2 (input) =
    convergeSequence input |> Seq.map (fun a -> a[0]) |> Seq.reduceBack (-)

let solve =
    let input = readInput "day09.txt"
                |> Array.map ((stringSplit ' ' StringSplitOptions.RemoveEmptyEntries) 
                    >> Array.map Int32.Parse)

    // Solve Part 1
    let resultPart1 = input |> Array.map solvePart1 |> Array.sum
    printfn "Part 1 Result: %A" resultPart1
    
    // Solve Part 2
    let resultPart2 = input |> Array.map solvePart2 |> Array.sum
    printfn "Part 2 Result: %A" resultPart2

