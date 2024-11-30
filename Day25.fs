module Day25

#nowarn "0025"
open Utils
open System


let parseEdges (edgeLine: string) =
    let parts = edgeLine.Split(':')
    let source = parts.[0].Trim()
    let targets = parts.[1].Trim().Split(' ')
    targets |> Array.map (fun target -> $"{source},{target}")

// Process all lines
let allEdges edges =
    edges
    |> List.collect (parseEdges >> Array.toList)


let solvePart1 (input) =
    let output = "Source,Target" :: (input |> Array.toList |> allEdges)
    IO.File.WriteAllLines("edges.csv", output)
    ""

let solvePart2 (input) =
    "Not implemented"

let solve =
    let input = readInput "day25.txt"

    // Solve Part 1
    let resultPart1 = solvePart1 input
    printfn "Part 1 Result: %A" resultPart1

    // Solve Part 2
    let resultPart2 = solvePart2 input
    printfn "Part 2 Result: %A" resultPart2

