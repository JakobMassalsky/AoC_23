module Day21

open Utils
open System
open System.Collections.Generic

let solvePart1 (input: Char array2d) m =
    let mutable start = (0, 0) 
    input |> Array2D.iteri (fun x y v -> if v = 'S' then start <- (x, y))
    let visited = Dictionary()
    let q = Queue()
    q.Enqueue((start, 0))

    while q.Count > 0 do
        let ((x, y), c) = q.Dequeue()
        if (c) < m then
            for (x', y') in [(x-1, y); (x+1, y); (x, y-1); (x, y+1)] do //(max (x-1) 0)..(min (x+1) (Array2D.length1 input - 1)) do
                // for y' in (max (y-1) 0)..(min (y+1) (Array2D.length2 input - 1)) do
                if x >=< (0, (Array2D.length1 input - 1)) && 
                    y >=< (0, (Array2D.length2 input - 1)) && 
                    not (visited.ContainsKey((x', y'))) && 
                    input[x', y'] <> '#'
                then q.Enqueue(((x', y'), c+1)); visited.Add((x', y'), c+1)

    visited.Values |> Seq.filter (fun v -> v%2 = m%2) |> Seq.length

let rec cellAut input =
    input |> Array2D.mapi (fun x y v ->
        if [for (x', y') in [(x-1, y); (x+1, y); (x, y-1); (x, y+1)] ->
                x' >=< (0, (Array2D.length1 input - 1)) && 
                y' >=< (0, (Array2D.length2 input - 1)) &&
                v <> '#' &&
                (input[x', y'] = 'O' || input[x', y'] = 'S')] |> List.exists id
            then 'O' else if v = '#' then '#' else '.')

// let numSteps = 26501365
let numSteps = 26501365
let solveMap input (entryPoint, remSteps): int64 =
    let visited = Dictionary()
    let q = Queue()
    q.Enqueue((entryPoint, remSteps))

    while q.Count > 0 do
        let ((x, y), c) = q.Dequeue()
        if c > 0 then
            for (x', y') in [(x-1, y); (x+1, y); (x, y-1); (x, y+1)] do
                if x' >=< (0, (Array2D.length1 input - 1)) && 
                    y' >=< (0, (Array2D.length2 input - 1)) && 
                    not (visited.ContainsKey((x', y'))) && 
                    input[x', y'] <> '#'
                then q.Enqueue(((x', y'), c+1)); visited.Add((x', y'), c+1)
    if remSteps < 0 then 0 else
    visited.Values |> Seq.filter (fun v -> v%2 = remSteps%2) |> Seq.length 
    |> int64

let solvePart2 (input: Char array2d) =
    let mutable start = (0, 0) 
    input |> Array2D.iteri (fun x y v -> if v = 'S' then start <- (x, y))
    let w, h = Array2D.length1 input, Array2D.length2 input
    let mutable locs = 0L
    let sx, sy = start

    let middle = solveMap input (start, numSteps)
    locs <- locs+middle

    let plotsEven = solveMap input ((0, 0), 0)
    let plotsOdd = solveMap input ((0, 0), 1)
    let straightStart = numSteps - (sx)
    let straightCopies = straightStart/w - 1
    // let leftPlots = solveMap input ((w-1, sy), straightStart)
    // let rightPlots = solveMap input ((0, sy), straightStart)
    // let topPlots = solveMap input ((sx, h-1), straightStart)
    // let bottomPlots = solveMap input ((sx, 0), straightStart)

    locs <- locs + solveMap input ((w-1, sy), straightStart - w*straightCopies - 1)
    locs <- locs + solveMap input ((0, sy), straightStart - w*straightCopies - 1)
    locs <- locs + solveMap input ((sx, h-1), straightStart - w*straightCopies - 1)
    locs <- locs + solveMap input ((sx, 0), straightStart - w*straightCopies - 1)

    let diagStart = numSteps - (sx*2)
    let diagCopies = (int64 straightCopies) * ((int64 straightCopies)-1L) / 2L

    // let trPlotsEven = solveMap input ((w-1, 0), diagStart)
    // let brPlotsEven = solveMap input ((w-1, h-1), diagStart)
    // let blPlotsEven = solveMap input ((0, h-1), diagStart)

    // let tlPlotsOdd = solveMap input ((0, 0), diagStart-1)
    // let trPlotsOdd = solveMap input ((w-1, 0), diagStart-1)
    // let brPlotsOdd = solveMap input ((w-1, h-1), diagStart-1)
    // let blPlotsOdd = solveMap input ((0, h-1), diagStart-1)

    let remSmall = int64 straightCopies + 1L
    let remBig = int64 straightCopies
    let startSmall = diagStart - h*straightCopies - 1
    let startBig = diagStart - h*(straightCopies-1) - 1

    let tl = solveMap input ((w-1, sy), startSmall) * remSmall +
                solveMap input ((w-1, sy), startBig) * remBig
    let tr = solveMap input ((0, sy), startSmall) * remSmall +
                solveMap input ((0, sy), startBig) * remBig
    let br = solveMap input ((sx, h-1), startSmall) * remSmall +
                solveMap input ((sx, h-1), startBig) * remBig
    let bl = solveMap input ((sx, 0), startSmall) * remSmall +
                solveMap input ((sx, 0), startBig) * remBig
    locs <- locs + tl + tr + br + bl
    // locs <- locs + tlPlots*diagCopies + tl
    // locs <- locs + trPlots*diagCopies + tr
    // locs <- locs + brPlots*diagCopies + br
    // locs <- locs + blPlots*diagCopies + bl

    locs

let solve =
    let input: Char array2d = readInput "day21.txt" |> inputToCharGrid

    // let m = iterate cellAut input 
    //         |> Seq.item 64 
    //         |> Seq.cast<Char>
    //         |> Seq.filter ((=) 'O')
    //         |> Seq.length

    // Solve Part 1
    let resultPart1 = solvePart1 input 64
    printfn "Part 1 Result: %A" resultPart1
    // test 15 -> 192
    // Solve Part 2
    let resultPart2 = solvePart2 input
    printfn "Part 2 Result: %A" resultPart2
    // < 622231110490586 // Naive
    // < 622237224805786 // smallBig Edges
    // > 582231110490586
