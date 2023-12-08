module Day08

open Utils
open System
open System.Text.RegularExpressions

let solvePart1 (input: Map<string,(string * string)>) (dirs: char array) =
    let mutable loc = "AAA"
    let mutable step = 0
    while loc <> "ZZZ" do
        let d = dirs[step % dirs.Length]
        loc <- if d = 'L' then fst input.[loc] else snd input.[loc]
        step <- step+1
    step

// based on multiple unjustified assumptions that are apparently supposed to be necessary bruh
let findLoopZs (input: Map<string,(string * string)>) (dirs: char array) (start: string) =
    let mutable loc = start
    let mutable step = 0
    let mutable stepLocations: Map<string, int> = Map.empty
    while step % dirs.Length <> 0 || not (stepLocations.ContainsKey(loc)) do
        if step % dirs.Length = 0 then (stepLocations <- stepLocations.Add(loc, step))
        let d = dirs[step % dirs.Length]
        loc <- if d = 'L' then fst input.[loc] else snd input.[loc]
        step <- step+1

    let loopLength = step - stepLocations[loc]
    loopLength

let solvePart2 (input: Map<string,(string * string)>) (dirs: char array) =
    input.Keys |> Seq.filter (fun s -> s.EndsWith('A')) 
        |> Seq.toArray
        |> Array.map (findLoopZs input dirs)
        |> Array.map int64 
        |> Array.reduce lcm

let solve =
    let input = readInput "day08.txt"
    let dirs = input[0].ToCharArray()
    let m = input 
            |> Array.skip 2 
            |> Array.map (fun s -> (new Regex("[0-9A-Z]+")).Matches(s) 
                                |> Seq.map (fun m -> m.Value) 
                                |> List.ofSeq
                                |> (fun (s :: l :: r) -> (s, (l, r.[0]))))
            |> Map.ofArray

    let resultPart1 = solvePart1 m dirs
    printfn "Part 1 Result: %A" resultPart1

    let resultPart2 = solvePart2 m dirs
    printfn "Part 2 Result: %A" resultPart2

