module Day02

open Utils
open System
open System.Text.RegularExpressions
// open FSharpx

let colorIndex = Map.empty.Add("red", 0).Add("green", 1).Add("blue", 2)

let addCube (cube: string): int list =
    let s = cube.Split(' ')
    [0;0;0] |> List.updateAt colorIndex.[s.[1]] (Int32.Parse s.[0])

let maxCubes (line: string): int list =
    line |> (new Regex(": |, |; ")).Split
        |> Array.skip 1
        |> Array.map addCube
        |> Array.fold (fun le re -> List.map2 (max) le re) [0;0;0]
    
let checkValid (cubes: int list): bool =
    cubes.[0] <= 12 && cubes.[1] <= 13 && cubes.[2] <= 14

let solvePart1 (input) =
    input |> Array.indexed
        |> Array.filter (fun (_, v) -> checkValid v)
        |> Array.sumBy (fun (i, _) -> i + 1)

let solvePart2 (input) =
    input |> Array.sumBy (fun v -> List.reduce (*) v)

let solve =
    let input = readInput "day02.txt" |> Array.map maxCubes
    // Solve Part 1
    let resultPart1 = solvePart1 input
    printfn "Part 1 Result: %A" resultPart1
    // Solve Part 2
    let resultPart2 = solvePart2 input
    printfn "Part 2 Result: %A" resultPart2
