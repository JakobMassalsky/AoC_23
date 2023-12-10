module Day10

open Utils
open System

let getDirs (c: char) = match c with
                        | '|' -> ((0, -1), (0, 1))
                        | '-' -> ((1, 0), (-1, 0))
                        | 'F' -> ((0, 1), (1, 0))
                        | '7' -> ((0, 1), (-1, 0))
                        | 'J' -> ((0, -1), (-1, 0))
                        | 'L' -> ((0, -1), (1, 0))
                        | _ -> ((0, 0), (0, 0))

let hasCounterDir (x, y) (d1, d2) =
    (-x, -y) = d1 || (-x, -y) = d2

let getNextDir (xo, yo) (d1, d2) = 
    if (-xo, -yo) = d1 then d2 else d1

let findNext (m: char array array) (x: int, y: int) (dx: int, dy: int) =
    let (nx, ny) = (x+dx, y+dy)
    ((nx, ny), m[ny][nx] |> getDirs |> (getNextDir (dx, dy)))

let solvePart1 (m: char array array) =
    let y = m |> Array.findIndex (Array.contains 'S')
    let x = m[y] |> Array.findIndex ((=) 'S')
    let dirs = [(0, 1);(0, -1);(-1, 0);(1, 0)]
                |> List.filter (fun (dx, dy) -> (m[clamp (y+dy) 0 140][clamp (x+dx) 0 140]) |> getDirs |> (hasCounterDir (dx, dy)))
                
    let p1 = ((x, y), dirs[0])
    let loopseq = p1 |> Seq.unfold (fun state -> Some (state, state |> tup (findNext m)))
    let sol = loopseq |> Seq.skip 1 |> Seq.findIndex (fun ((xi, yi),_) -> xi = x && yi = y)
    let loopSet = loopseq |> Seq.take 20000 |> Seq.map fst |> Set.ofSeq
    ((sol+1) / 2, loopSet)
    // Why is this so much longer than Seq.unfold its so cool??
    // let rec loopseq point = seq { 
    //     yield point
    //     yield! point |> tup (findNext m) |> loopseq
    // }

let updateState c inLoopSet (inside, from) =
    if not inLoopSet then (inside, from)
    else match c with
            | '|' -> (not inside, 0)
            | 'F' -> (inside, 1)
            | 'L' -> (inside, -1)
            | '7' -> ((if from = 1 then inside else not inside), 0)
            | 'J' -> ((if from = -1 then inside else not inside), 0)
            | _ -> (inside, from)

let traverseLine (loopSet: Set<int * int>) y (line: char array) =
    let mutable insideLoop = (false, 0)
    [|for x in 0..line.Length - 1 -> 
        let inLoopSet = (Set.contains (x, y) loopSet)
        insideLoop <- updateState line[x] inLoopSet insideLoop
        if fst insideLoop && not inLoopSet then true else false|]

let solvePart2 (input: char array array) (loopSet: Set<int * int>) =
    input |> Array.mapi (traverseLine loopSet) |> Array.sumBy (Array.filter id >> Array.length) // Array.map (Array.map (fun i -> if i then 'I' else '.')) // 

let solve =
    let input = readInput "day10.txt" |> Array.map (fun s -> s.ToCharArray())

    // Solve Part 1
    let (resultPart1, loopSet) = solvePart1 input
    printfn "Part 1 Result: %A" resultPart1

    let input = readInput "day10.txt" |> Array.map (fun s -> s.Replace('S', '|').ToCharArray())

    // Solve Part 2
    let resultPart2 = (solvePart2 input loopSet)// - loopSet.Count
    printfn "Part 2 Result: %A" resultPart2

