module Day03

open Utils
open System
open System.Text.RegularExpressions
open MathNet.Numerics

type eNumber = 
    struct
        val xMin: int
        val xMax: int
        val y: int
        val v: int
        new(index: int, y: int, string: String) = 
            { xMin = index; xMax = index + string.Length - 1; y = y; v = Int32.Parse string }

        member this.checkValid(grid: char[,]): bool =
            grid |> paddedSlice (this.xMin, this.y) (this.xMax, this.y)
                |> Seq.cast<Char>
                |> Seq.exists (fun v -> not (v = '.') && not (Char.IsDigit v) )
    end
    
let solvePart1 (input: string array) (grid: char[,]) (numbers: eNumber array) =
    numbers |> Array.sumBy (fun n -> if n.checkValid(grid) then n.v else 0)

let findAdjacentNumbers x y v (numbers: eNumber array) =
    if v = '*'
    then numbers 
        |> Array.filter (fun n ->
            y >=< (n.xMin-1, n.xMax+1) && x >=< (n.y-1, n.y+1))
        |> (fun num -> if num.Length = 2 then num[0].v * num[1].v else 0)
    else 0

let solvePart2 (input: string array) (grid: char[,]) (numbers: eNumber array) =
    grid |> Array2D.mapi (fun x y v -> findAdjacentNumbers x y v numbers)
        |> Seq.cast<int>
        |> Seq.sum

let findMatchesAndIndices (pattern: string) (input: string) =
    (new Regex(pattern)).Matches(input)
        |> Seq.map (fun m -> (m.Value, m.Index))
        |> Array.ofSeq

let solve =
    let input = readInput "day03.txt"

    let numbers = input |> Array.map (findMatchesAndIndices "\d+") 
                    |> Array.mapi (fun y line -> 
                        Array.map (fun (v, x) -> new eNumber(x, y, v)) line)
                    |> Array.concat

    let grid = inputToCharGrid input

    // Solve Part 1
    let stopWatch = System.Diagnostics.Stopwatch.StartNew()
    let resultPart1 = solvePart1 input grid numbers
    stopWatch.Stop()
    printfn "Part 1 Result: %A" resultPart1
    printfn "%fms" stopWatch.Elapsed.TotalMilliseconds

    // Solve Part 2
    let resultPart2 = solvePart2 input grid numbers
    printfn "Part 2 Result: %A" resultPart2
