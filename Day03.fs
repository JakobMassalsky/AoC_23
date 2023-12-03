module Day03

open Utils
open System
open System.Text.RegularExpressions

type eNumber = 
    struct
        val xMin: Int32
        val xMax: Int32
        val y: Int32
        val v: Int32
        new(index: Int32, y: Int32, string: String) = 
            { xMin = index; xMax = index + string.Length - 1; y = y; v = Int32.Parse string }

        member this.checkValid(grid: char[,]): bool =
            grid[max 0 (this.y-1) .. min (Array2D.length1 grid - 1) (this.y+1), 
                max 0 (this.xMin-1) .. min (Array2D.length2 grid - 1) (this.xMax+1)]
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

    let numbers = input |> Array.map (fun line -> (findMatchesAndIndices "\d+" line)) 
                    |> Array.mapi (fun y line -> 
                        Array.map (fun (v, x) -> new eNumber(x, y, v)) line)
                    |> Array.concat

    // Map each line to an array of characters
    let arrays = input |> Array.map Array.ofSeq
    // Convert the sequence of arrays into a 2D array
    let rows = arrays.Length
    let cols = if rows > 0 then arrays.[0].Length else 0
    let grid = Array2D.init rows cols (fun i j -> arrays.[i].[j])

    // Solve Part 1
    let resultPart1 = solvePart1 input grid numbers
    printfn "Part 1 Result: %A" resultPart1

    // Solve Part 2
    let resultPart2 = solvePart2 input grid numbers
    printfn "Part 2 Result: %A" resultPart2
