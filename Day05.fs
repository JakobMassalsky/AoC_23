module Day05

open Utils
open System
open System.Text.RegularExpressions

// let changeOff (r: cRange) off =
//     new cRange(r.)

// let rangeIntersect2 (r1: cRange) (r2: cRange) =
//     if r2.st <= r1.en || r1.en <= r2.st then [ r2 ]
//     else if r1.st <= r2.st && r1.en >= r2.en then 
//         r2.off <- r2.off + r1.off r2

let makeConversion n ([|dR; sR; l|]: int64 array) =
    if n >=< (sR, sR+l-int64(1)) then n - (sR - dR) else n

let oneConversion (m: int64 array array) n: int64 =
    m |> Array.map (makeConversion n)
        |> Array.tryFind ((<>) n)
        |> Option.defaultValue n

let solvePart1 (input: int64 array array) =
    let seeds = input[0]
    let conversions = input |> Array.skip 1
                    |> Array.map (Array.chunkBySize 3)
    seeds |> Array.map (fun s -> Array.fold (fun state conv -> oneConversion conv state) s conversions)
        |> Array.min

let solvePart2 (input: int64 array array): int64 =
    let seeds = 
        input[0] |> Array.chunkBySize 2
        |> Array.map (fun [|v; l|] -> Array.init (int32 l) (fun i -> int64(i) + v))

    printfn "input: %A" (seeds[0][0])
    let conversions = input |> Array.skip 1
                    |> Array.map (Array.chunkBySize 3)
    seeds |> Array.map (fun ss -> 
                ss |> Array.map (fun s -> Array.fold (fun state conv -> oneConversion conv state) s conversions)
                    |> Array.min)
            |> Array.min

let solve =
    let input = 
        slurpInput "day05.txt"
        |> (fun s -> s.Split("\r\n\r\n"))
        |> Array.map (fun s -> s |> (new Regex("\d+")).Matches |> Seq.toArray |> Array.map (fun m -> m.Value |> Int64.Parse))
    
    // Solve Part 1
    let resultPart1 = solvePart1 input
    printfn "Part 1 Result: %A" resultPart1
    
    // Solve Part 2
    let resultPart2 = solvePart2 input
    printfn "Part 2 Result: %A" resultPart2

