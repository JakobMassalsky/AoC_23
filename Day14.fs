module Day14

open Utils
open System
open MathNet.Numerics.LinearAlgebra

let solvePlace (vec: double array) rev i (v: double) =
    if v = double -1 then double -1 else
    let lastR = vec[..i-1] |> Array.tryFindIndexBack ((=) (double -1)) |> Option.defaultValue (- 1) |> (+) 1
    let nextR = vec[i+1..] |> Array.tryFindIndex ((=) (double -1)) |> Option.defaultValue (vec.Length-1-i) |> (+) (i)
    let numRocks = vec[lastR..nextR] |> Array.filter ((=) (double 1)) |> Array.length
    match rev with
    | true -> if nextR - i < numRocks then double 1 else double 0
    | false -> if i - lastR < numRocks then double 1 else double 0

let solveCol rev i (col: double Vector) =
    col |> Vector.mapi (solvePlace (col.AsArray()) rev)
    

let doSingle (input: double Matrix) =
    input |> Matrix.mapCols (solveCol false)

let doCycle (input: double Matrix) =
    input |> Matrix.mapCols (solveCol false)
        |> Matrix.mapRows (solveCol false)
        |> Matrix.mapCols (solveCol  true)
        |> Matrix.mapRows (solveCol true)

let scoreMap m = m |> Matrix.mapi (fun x _ v -> if v = (double 1) then double (m.RowCount - x) else double 0) |> Matrix.sum |> int

let solvePart1 (input) =
    input |> doSingle |> scoreMap

let solvePart2 (input: double Matrix) =
    let mutable dict = Map.empty
    let (i1, i2) = input |> iterate doCycle |> Seq.mapi
                    (fun i v -> match Map.tryFind (v.ToArray()) dict with
                                | Some(v) -> (v, i)
                                | None -> dict <- dict.Add((v.ToArray()), i); (-1, i))
                    |> Seq.find (fst >> (<) 0)

    // Clarity >> Golfing :eyes:
    let cLength = i2 - i1
    let cDur = 1000000000 - i1
    let rem = cDur % cLength
    let i = i1 + rem
    input |> iterate doCycle |> Seq.item (i) |> scoreMap

let solve =
    let input = readInput "day14.txt" |> inputToCharGrid
                |> Array2D.map (fun c -> match c with
                                            | '.' -> double 0
                                            | 'O' -> double 1
                                            | _ -> double -1)
                |> DenseMatrix.ofArray2

    // Solve Part 1
    let resultPart1 = solvePart1 input
    printfn "Part 1 Result: %A" resultPart1

    // Solve Part 2
    let resultPart2 = solvePart2 input
    printfn "Part 2 Result: %A" resultPart2

