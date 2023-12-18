module Day18

#nowarn "0025"
open Utils
open System
open System.Text.RegularExpressions

let getDir d =
    match d with
    | 'R' | '0' -> (1, 0)
    | 'L' | '2' -> (-1, 0)
    | 'D' | '1' -> (0, 1)
    | _ -> (0, -1)

let parseP1 (s: string) =
    let dir :: dist :: _ = s.Split(' ') |> Array.toList
    getDir (Seq.head dir) |> mapBoth ((*) (Int32.Parse dist))

let parseP2 (s: string) =
    let _ :: _ :: [hex] = s.Split(' ') |> Array.toList
    let dist = hex[0..4] |> fun s -> Convert.ToInt32(s, 16) 
    getDir (Seq.last hex) |> mapBoth ((*) dist)

let area parseFunc (input) =
    input |> Array.toList 
    |> List.scan (fun s (v: string) -> applyTup (+) s (parseFunc v)) (0,0)
    |> List.map (mapBoth int64)
    |> List.pairwise 
    |> List.sumBy (fun ((x1, y1), (x2, y2)) -> x1*y2 - y1*x2 + (abs(x2-x1)) + (abs(y2-y1))) 
    |> (fun v -> v >>> 1) |> (+) 1L

let solve =
    let input = readInput "day18.txt" |> Array.map (fun s -> (new Regex("[\(\)#]")).Replace(s, ""))

    // Solve Part 1
    let resultPart1 = area parseP1 input
    printfn "Part 1 Result: %A" resultPart1

    // Solve Part 2
    let resultPart2 = area parseP2 input
    printfn "Part 2 Result: %A" resultPart2

