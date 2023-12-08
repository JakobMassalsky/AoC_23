module Day08

open Utils
open System
open System.Text.RegularExpressions

let nextNode (input: Map<string,(string * string)>) d loc =
    if d = 0 then fst input.[loc] else snd input.[loc]

let solvePart1 (input: Map<string,(string * string)>) (dirs: int array) =
    let mutable loc = "AAA"
    let mutable step = 0
    while loc <> "ZZZ" do
        let d = dirs[step % dirs.Length]
        loc <- if d = 0 then fst input.[loc] else snd input.[loc]
        step <- step+1
    // printfn "path: %A" (Seq.take 2 (path dirs[0] "AAA"))
    step

let solvePart2 (input: Map<string,(string * string)>) (dirs: int array) (starts: string array): int =
    let mutable startsLoc = starts
    let mutable step = 0
    while ((Array.filter (fun (s: string) -> s.EndsWith('Z')) startsLoc).Length <> starts.Length) do
        let s = step % dirs.Length
        let d = dirs[s]
        startsLoc <- startsLoc |> Array.map (fun loc -> if d = 0 then fst input.[loc] else snd input.[loc])
        // printfn "Locations: %A" startsLoc
        step <- step+1
    // printfn "path: %A" (Seq.take 2 (path dirs[0] "AAA"))
    step

let solve =
    let input = readInput "day08.txt"
    let dirs = input[0].ToCharArray() |> Array.map (fun d -> if d = 'L' then 0 else 1)

    let nodes = input 
                |> Array.skip 2 
                |> Array.map (fun s -> (new Regex("[0-9A-Z]+")).Matches(s) 
                                    |> Seq.map (fun m -> m.Value) |> List.ofSeq
                                    |> (fun (s :: l :: r )-> (s, (l, r.[0]))))
    let m: Map<string,(string * string)> = Map.ofArray nodes
    printfn "Map: %A" m
    // printfn "Dirs: %A" dirs
    // m.["AAA"]
    // Solve Part 1
    // let resultPart1 = solvePart1 m dirs
    // printfn "Part 1 Result: %A" resultPart1

    let p2 = m.Keys 
                |> Seq.filter (fun s -> s.EndsWith('A')) 
                |> (fun s -> solvePart2 m dirs (Seq.toArray s))

    // Solve Part 2
    let resultPart2 = p2
    printfn "Part 2 Result: %A" resultPart2

