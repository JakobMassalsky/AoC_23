module Day17

open Utils
open System
open System.Collections.Generic
open MathNet.Numerics.LinearAlgebra

let getNeighbours (grid: double Matrix) (mi, ma) ((x, y), (dx, dy)) =
    [for d in mi..ma -> ((x+(dy*d), y+(dx*d)),(dy, dx))] @ [for d in mi..ma -> ((x-(dy*d), y-(dx*d)),(-dy, -dx))]
    |> List.filter (fun ((x', y'), _) -> x' >=< (0, grid.ColumnCount-1) && y' >=< (0, grid.RowCount-1))
    |> List.map (fun ((x', y'), d) -> 
        (((x', y'), d), grid[(min x x')..(max x x'), (min y y')..(max y y')] 
            |> Matrix.sum  
            |> ((+) -grid[x, y])
            |> int))

let bfs nFunc (start: int*int) =
    let visited = Dictionary<_,_>()
    let queue = PriorityQueue<_,_>()
    queue.Enqueue((start, (1,0), 0), 0)
    queue.Enqueue((start, (0,1), 0), 0)
    
    while queue.Count > 0 do
        let (loc, dir, cost) = queue.Dequeue()

        for ((loc', dir'), cost') in nFunc (loc, dir) do
            let newKey = (loc', dir')
            let newCost = cost' + cost
            match visited.ContainsKey(newKey) with
            | true -> if newCost < visited[newKey] then (visited[newKey] <- newCost; queue.Enqueue((loc', dir', newCost), newCost))
            | false -> visited.Add(newKey, newCost); queue.Enqueue((loc', dir', newCost), newCost)

    visited

let solvePart1 (input: double Matrix) =
    bfs (getNeighbours input (1,3)) (0,0)
    |> fun d -> min d[(140,140), (1,0)] d[(140,140), (0,1)]

let solvePart2 (input: double Matrix) =
    bfs (getNeighbours input (4,10)) (0,0)
    |> fun d -> min d[(140,140), (1,0)] d[(140,140), (0,1)]

let solve =
    let input: Matrix<Double> = readInput "day17.txt" |> inputToIntGrid

    // Solve Part 1
    let resultPart1 = solvePart1 input
    printfn "Part 1 Result: %A" resultPart1

    // Solve Part 2
    let resultPart2 = solvePart2 input
    printfn "Part 2 Result: %A" resultPart2

