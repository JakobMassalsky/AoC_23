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

let numSteps = 26501365
// let numSteps = 1180148
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
                then q.Enqueue(((x', y'), c-1)); visited.Add((x', y'), c-1)
    if remSteps < 0 then 0 else
    visited.Values |> Seq.filter (fun v -> v%2 = 0) |> Seq.length 
    |> int64

let solvePart2 (input: Char array2d) =
    let mutable start = (0, 0) 
    input |> Array2D.iteri (fun x y v -> if v = 'S' then start <- (x, y))
    let w, h = Array2D.length1 input, Array2D.length2 input
    let mutable locs = 0L
    let sx, sy = start

    let plotsOdd = solveMap input (start, numSteps)
    let plotsEven = solveMap input (start, numSteps-1)
    // locs <- locs+plotsEven

    p (plotsEven, plotsOdd)

    let straightStart = numSteps - (sx)


    // p (straightStart - w*straightCopies)

    let diagStart = numSteps - w
    // let diagCopies = (int64 straightCopies) * ((int64 straightCopies)-1L) / 2L

    // let evenCopiesDiag = (pown ((int64 straightCopies - 1L)) 2)
    // let oddCopiesDiag = ((int64 straightCopies - 1L) / 2L + 1L) * ((int64 straightCopies - 1L) / 2L) * 4L

    let n = int64 (straightStart / w) - 1L
    let evens = if n % 2L = 0 then pown (n) 2 else pown (n+1L) 2
    let odds = if n % 2L = 0 then pown (n+1L) 2 else pown (n) 2 
    // let evens = pown (n-1L) 2
    // let odds = (pown n 2)
    let nint = n |> int

    p (n, odds, evens)

    // let oddCopiesStraight = ((int64 straightCopies) / 2L + 1L) * 4L
    // let evenCopiesStraight = ((int64 straightCopies) / 2L) * 4L

    locs <- locs + evens*plotsEven// (oddCopiesDiag * plotsOdd) + (oddCopiesStraight * plotsOdd)
    locs <- locs + odds*plotsOdd// (evenCopiesDiag * plotsEven) + (evenCopiesStraight * plotsEven)
    // p locs
    // p (straightCopies * 4 + 1, oddCopiesStraight + evenCopiesStraight)

    let remSmall = n+1L
    let remBig = n
    p (remBig, remSmall)
    let startSmall = diagStart - h*nint - 1
    let startBig = diagStart - h*(nint-1) - 1
    p (startSmall, startBig)
    let tl = solveMap input ((0, 0), startSmall) * remSmall +
                solveMap input ((0, 0), startBig) * remBig
    let tr = solveMap input ((w-1, 0), startSmall) * remSmall +
                solveMap input ((w-1, 0), startBig) * remBig
    let br = solveMap input ((w-1, h-1), startSmall) * remSmall +
                solveMap input ((w-1, h-1), startBig) * remBig
    let bl = solveMap input ((0, h-1), startSmall) * remSmall +
                solveMap input ((0, h-1), startBig) * remBig
    locs <- locs + tl + tr + br + bl
    p (solveMap input ((0, 0), startBig))

    // p locs

    locs <- locs + solveMap input ((w-1, sy), straightStart - w*nint-1)
    locs <- locs + solveMap input ((0, sy), straightStart - w*nint-1)
    locs <- locs + solveMap input ((sx, h-1), straightStart - w*nint-1)
    locs <- locs + solveMap input ((sx, 0), straightStart - w*nint-1)


    locs

let solve =
    let input: Char array2d = readInput "day21.txt" |> inputToCharGrid

    // let m = iterate cellAut input 
    //         |> Seq.item 133 
    //         |> Seq.cast<Char>
    //         |> Seq.filter ((=) 'O')
    //         |> Seq.length

    // let mutable start = (0, 0) 
    // input |> Array2D.iteri (fun x y v -> if v = 'S' then start <- (x, y))
    // Solve Part 1
    // let resultPart1 = solvePart1 input 64
    // printfn "Part 1 Result: %A" resultPart1

    // Test:
    // 7 	    52
    // 8 	    68
    // 25 	    576
    // 42 	    1576
    // 59 	    3068
    // 76 	    5052
    // 1180148 	1185525742508 

    // Solve Part 2
    let resultPart2 = solvePart2 input
    printfn "Part 2 Result: %A" resultPart2
