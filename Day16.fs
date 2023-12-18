module Day16

#nowarn "0025"
open Utils
open System

let getNewDir (dx, dy) c =
    match dx, dy, c with
    | _, _, '\\' -> [(dy, dx)]
    | _, _, '/' -> [(-dy, -dx)]
    | _, 0, '|' -> [(0, 1); (0, -1)]
    | 0, _, '-' -> [(1, 0); (-1, 0)]
    | _, _, _ -> [(dx, dy)]

let rec advanceRay (grid: Char array2d) (loc: int*int) (d: int*int) (visited: (int*int) Set) =
    let mutable vis = visited
    let mutable cur = loc
    let mutable dir = d
    while fst cur >=< (0, grid.GetLength(1)-1) &&
        snd cur >=< (0, grid.GetLength(0)-1) do
        let nd :: rd = getNewDir dir grid[snd cur, fst cur]
        dir <- nd
        match rd with
        | [nd2] when not (vis.Contains(cur)) -> vis <- advanceRay grid cur nd2 vis
        | _ -> vis <- vis

        // match getNewDir dir grid[snd cur, fst cur] with
        // | [nd; nd2] when not (vis.Contains(cur)) -> (vis <- advanceRay grid cur nd2 vis); (dir <- nd)
        // | [nd] -> (dir <- nd)
        // | _ -> cur <- cur

        vis <- vis.Add(cur)
        cur <- (fst cur + fst dir, snd cur + snd dir)
    vis

let solvePart1 (input: Char array2d) =
    advanceRay input (0, 0) (1, 0) Set.empty |> Set.count

let solvePart2  (input: Char array2d) =
    let pos1 = Array.init (input.GetLength(1)) (fun x -> (x, 0))
    let pos2 = Array.init (input.GetLength(0)) (fun y -> (0, y))
    let pos3 = Array.init (input.GetLength(1)) (fun x -> (x, (input.GetLength(0))-1))
    let pos4 = Array.init (input.GetLength(0)) (fun y -> ((input.GetLength(1))-1, y))
    let m1 = pos1 |> Array.Parallel.map (fun p -> advanceRay input p (0, 1) Set.empty |> Set.count) |> Array.max
    let m2 = pos2 |> Array.Parallel.map (fun p -> advanceRay input p (1, 0) Set.empty |> Set.count) |> Array.max
    let m3 = pos3 |> Array.Parallel.map (fun p -> advanceRay input p (0, -1) Set.empty |> Set.count) |> Array.max
    let m4 = pos4 |> Array.Parallel.map (fun p -> advanceRay input p (-1, 0) Set.empty |> Set.count) |> Array.max
    Array.max [|m1; m2; m3; m4|]

let solve =
    let input: Char array2d = readInput "day16.txt" |> inputToCharGrid


    // Solve Part 1
    let resultPart1 = solvePart1 input
    printfn "Part 1 Result: %A" resultPart1

    // // Solve Part 2
    // let resultPart2 = solvePart2 input
    // printfn "Part 2 Result: %A" resultPart2

    let stopWatch = System.Diagnostics.Stopwatch.StartNew()
    let resultPart1 = solvePart2 input
    stopWatch.Stop()
    printfn "Part 1 Result: %A" resultPart1
    printfn "%fms" stopWatch.Elapsed.TotalMilliseconds

