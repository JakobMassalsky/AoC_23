module Day11

open Utils
open System

let galaxyCols galaxySet (x1: int64) (x2: int64) =
    galaxySet |> Set.map fst 
        |> Set.filter (fun xt -> xt >=< ((min x1 x2), (max x1 x2)))
        |> Set.count |> int64

let galaxyRows galaxySet y1 y2 =
    galaxySet |> Set.map snd 
        |> Set.filter (fun yt -> yt >=< ((min y1 y2), (max y1 y2)))
        |> Set.count |> int64

let getDist galaxySet scaleFactor (x1, y1) (x2, y2) = 
    let dx = int64(abs (x2-x1))
    let dy = int64(abs (y2-y1))
    let emptyCols = dx - (galaxyCols galaxySet x1 x2) + 1L
    let emptyRows = dy - (galaxyRows galaxySet y1 y2) + 1L
    dx + ((scaleFactor-1L)*emptyCols) + dy + ((scaleFactor-1L)*emptyRows)

let solve =
    let input = readInput "day11.txt" |> inputToCharGrid

    let mutable galaxySet = Set.empty
    input |> Array2D.iteri (fun x y c -> if c = '#' then galaxySet <- galaxySet.Add(int64(x), int64(y)))

    let dists = galaxySet |> Set.toArray |> (fun x -> (x, x)) ||> Array.allPairs

    // Solve Part 1
    printfn "Part 1 Result: %A" ((dists |> Array.sumBy (tup (getDist galaxySet 2L))) / 2L)

    // Solve Part 2
    printfn "Part 2 Result: %A" ((dists |> Array.sumBy (tup (getDist galaxySet 1000000L))) / 2L)

