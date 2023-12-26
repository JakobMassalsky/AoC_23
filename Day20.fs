module Day20

#nowarn "0025"
open Utils
open System
open System.Collections.Generic

let invFlipFlop (m: bool option) =
    match m with
    | Some b -> Some (not b)
    | _ -> None

let updateMem pulse source (m: Map<string,bool> option) =
    match m with
    | Some ma -> ma |> Map.change source (fun _ -> Some pulse) |> Some
    | None -> None

let pressButton ((memory: Map<string,Map<string,bool>>), (fStates: Map<string,bool>), (ops: Map<string,char>), (dests: Map<string,string array>), h, l, rx, find) =
    let mutable mem = memory
    let mutable fstates = fStates
    let mutable queue = Queue()
    queue.Enqueue(("roadcaster", false, "button"))
    let mutable highs = h
    let mutable lows = l
    let mutable rx' = rx
    // p dests
    while queue.Count > 0 do
        let (m, pulse, source) = queue.Dequeue()
        // p (source, pulse, "->", m)
        if pulse then highs <- highs+1 else lows <- lows+1
        if m = find && not pulse then rx' <- rx' + 1 else rx' <- rx'
        if not (ops.ContainsKey(m)) then rx' <- rx' else
        match ops[m] with
        | '%' -> if not pulse 
                    then fstates <- Map.change m invFlipFlop fstates; 
                            for d in dests[m] do
                                queue.Enqueue((d, fstates[m], m))
                    else fstates <- fstates;
        | '&' -> mem <- Map.change m (updateMem pulse source) mem;
                    for d in dests[m] do
                        queue.Enqueue((d, mem[m] |> Map.values |> Seq.exists ((=) false), m))
        | 'b' -> for d in dests[m] do
                    queue.Enqueue((d, pulse, m))
        | _ -> highs <- highs
    mem, fstates, ops, dests, highs, lows, rx', find

let solvePart1 memory ops fStates dests =
    let (_, _, _, _, h, l, _, _) = iterate pressButton (memory, fStates, ops, dests, 0, 0, 0, "") |> Seq.item 1000
    l * h

let solvePart2 memory ops fStates dests =
    let i = iterate pressButton (memory, fStates, ops, dests, 0, 0, 0, "qz")
    memory["qn"].Keys |> Seq.map (fun k -> 
        iterate pressButton (memory, fStates, ops, dests, 0, 0, 0, k) |> Seq.findIndex (fun (_,_,_,_,_,_,rx, _) -> rx >= 1))
        |> Seq.map int64
        |> Seq.reduce lcm

let solve =
    let input = readInput "day20.txt"
                |> Array.map (fun s -> s.Split(" -> "))

    let dests = input 
                |> Array.map (fun [|s; d|] -> (s[1..], d.Split(", ")))
                |> Map.ofArray

    let ops = input 
                |> Array.map (fun [|s; _|] -> (s[1..], s[0]))
                |> Map.ofArray

    let fStates = input 
                |> Array.filter (fun [|s; _|] -> (s[0] = '%'))
                |> Array.map (fun [|s; _|] -> (s[1..], false))
                |> Map.ofArray

    let memory = input 
                |> Array.filter (fun [|s; _|] -> (s[0] = '&'))
                |> Array.map (fun [|s; _|] -> (s[1..], dests 
                                                |> Map.filter (fun k v -> Array.contains s[1..] v)
                                                |> Map.keys
                                                |> Seq.map (fun k -> (k, false))
                                                |> Map.ofSeq))
                |> Map.ofArray

    // Solve Part 1
    let resultPart1 = solvePart1 memory ops fStates dests
    printfn "Part 1 Result: %A" resultPart1

    // Solve Part 2
    let resultPart2 = solvePart2 memory ops fStates dests
    printfn "Part 2 Result: %A" resultPart2

