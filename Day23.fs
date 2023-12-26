module Day23

open Utils
open System

let neighboursPart1 (input: char array2d) (x, y) =
    [(x-1, y); (x+1, y); (x, y-1); (x, y+1)] 
        |> List.filter (fun (x', y') -> 
            match input[x', y'] with
            | '.' -> true
            | '>' -> y' - y = 1
            | '<' -> y' - y = -1
            | 'v' -> x' - x = 1
            | '^' -> x' - x = -1
            | _ -> false)

let neighboursPart2 (input: char array2d) (x, y) =
    [(x-1, y); (x+1, y); (x, y-1); (x, y+1)] 
        |> List.filter (fun (x', y') -> input[x', y'] <> '#')

let shortcut finish fNb (start: int * int) (last: int * int) = 
    let rec loop current last length =
        if current = finish then (current, length) else
        match fNb current |> List.filter ((<>) last) with
        | [] -> (current, -1)
        | [newPos] -> loop newPos current (length+1)
        | _ -> (current, length)
    
    loop start last 1

let rec dfs (nodes: Map<_,_>) (seen: (int * int) Set) finish (current: int) (loc: int * int) =
    if loc = finish then current else
    match nodes[loc] |> List.filter (fst >> (fun v -> Set.contains v seen) >> not) with
    | [] -> -1
    | nb -> 
        nb |> List.map (fun (n, cost) -> dfs nodes (seen.Add(n)) finish (current+cost) n) 
        |> List.max

let makeGraph input fNb = 
    let h = input |> Array2D.length1
    let w = input |> Array2D.length2
    let rec loop nodes (graph: Map<_,_>) =
        match nodes with
        | [] -> graph
        | node :: rest ->
            if graph.ContainsKey(node) then loop rest graph else
            let nbs = fNb node
            let nextInters = nbs |> List.map (fun n -> shortcut (w-2, h-2) fNb n node)
                            |> List.filter (snd >> (<=) 0)
            loop (rest @ (nextInters |> List.map fst)) (graph.Add(node, nextInters))
    loop [(1, 1)] Map.empty

let solveWith (input: char array2d) fNb =
    let h = input |> Array2D.length1
    let w = input |> Array2D.length2
    dfs (makeGraph input (fNb input)) (Set.singleton (1, 1)) (w-2, h-2) 0 (1, 1)
    + 2

let solve =
    let input = readInput "day23.txt" |> inputToCharGrid

    // Solve Part 1
    let resultPart1 = solveWith input neighboursPart1
    printfn "Part 1 Result: %A" resultPart1

    // Solve Part 2
    let resultPart2 = solveWith input neighboursPart2
    printfn "Part 2 Result: %A" resultPart2

