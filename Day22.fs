module Day22

open Utils
open System

let solvePart1 (input: (int array * int array) array) =
    let mutable cubeGrid = Map.empty
    let mutable necessaryBricks = Map.empty
    input |> Array.sortBy (fun (s1, _) -> s1[2]) 
        |> Array.iteri (fun ind (s, e) -> 
            if s[2] <> e[2] 
            then [0..s[2]] |> List.findBack (fun i -> cubeGrid.ContainsKey(s[0], s[1], i-1) || i = 0)
                    |> fun i ->
                        if i > 0 then necessaryBricks <- necessaryBricks.Add(cubeGrid[s[0], s[1], i-1], ind)
                        for z in i..(i+(e[2]-s[2])) do 
                            cubeGrid <- cubeGrid.Add((s[0], s[1], z), ind)
            else if s[1] <> e[1] 
            then [0..s[2]] |> List.findBack (fun i -> [s[1]..e[1]] |> Seq.exists (fun y -> cubeGrid.ContainsKey(s[0], y, i-1)) || i = 0)
                    |> fun i -> 
                        [s[1]..e[1]] |> List.filter (fun y -> cubeGrid.ContainsKey(s[0], y, i-1)) 
                            |> List.map (fun y -> cubeGrid[s[0], y, i-1])
                            |> List.distinct
                            |> (fun l -> if l.Length = 1 then necessaryBricks <- necessaryBricks.Add(l[0], ind))
                            // |> List.iter (fun v -> necessaryBricks <- necessaryBricks.Add(v, ind))
                        for y in s[1]..e[1] do 
                            cubeGrid <- cubeGrid.Add((s[0], y, i), ind)
            else [0..s[2]] |> List.findBack (fun i -> [s[0]..e[0]] |> Seq.exists (fun x -> cubeGrid.ContainsKey(x, s[1], i-1)) || i = 0)
                    |> fun i -> 
                        [s[0]..e[0]] |> List.filter (fun x -> cubeGrid.ContainsKey(x, s[1], i-1)) 
                            |> List.map (fun x -> cubeGrid[x, s[1], i-1])
                            |> List.distinct
                            |> (fun l -> if l.Length = 1 then necessaryBricks <- necessaryBricks.Add(l[0], ind))
                            // |> List.iter (fun v -> necessaryBricks <- necessaryBricks.Add(v, ind))
                        for x in s[0]..e[0] do 
                            cubeGrid <- cubeGrid.Add((x, s[1], i), ind))
    
    // cubeGrid
    // [0..input.Length-1] |> List.filter (fun i -> )
    input.Length - necessaryBricks.Count

let solvePart2 (input) =
    "Not implemented"

let solve =
    let input = readInput "day22.txt"
                |> Array.map (fun s -> s.Split('~') |> Array.map (fun s -> s.Split(',') |> Array.map Int32.Parse) |> a2t)

    // Solve Part 1
    let resultPart1 = solvePart1 input
    printfn "Part 1 Result: %A" resultPart1

    // Solve Part 2
    let resultPart2 = solvePart2 input
    printfn "Part 2 Result: %A" resultPart2

