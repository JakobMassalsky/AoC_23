module Day13

open Utils
open System
open MathNet.Numerics.LinearAlgebra

let toGrid (tile: string) =
    tile |> (fun s -> s.Replace('.', '0').Replace('#', '1').Split("\r\n")) |> inputToIntGrid

let checkLine (tile: Double Matrix) (i) =
    let c = tile.RowCount
    if i >= c then 0
    else
    let d = min (i) (c-i)
    let res = {0..d-1} |> Seq.tryFind (fun col -> tile[i+col,*] <> tile[i-col-1,*])
    match res with
    | Some(_) -> 0
    | None -> i

let checkColumn (tile: Double Matrix) (i) =
    let c = tile.ColumnCount
    if i >= c then 0
    else
    let d = min (i) (c-i)
    let res = {0..d-1} |> Seq.tryFind (fun col -> tile[*,i+col] <> tile[*,i-col-1])
    match res with
    | Some(_) -> 0
    | None -> i

let findOld (tile: Double Matrix) =
    let s = tile.RowCount
    let ir = [|1..s-1|] |> (fun a -> Array.FindAll(a, checkLine tile >> (<) 0))
    let s = tile.ColumnCount
    let ic = [|1..s-1|] |> (fun a -> Array.FindAll(a, checkColumn tile >> (<) 0))
    ir, ic

let findAlternateMirrorLine (tile: Double Matrix) (ir: int array, ic: int array) x y v: Double =
    let t = DenseMatrix.zero tile.RowCount tile.ColumnCount
    tile.CopyTo(t)
    t[x, y] <- ((double 1)-v)
    let irn, icn = findOld t
    let irn = irn |> Array.filter (fun v -> not (Array.contains v ir))
    let icn = icn |> Array.filter (fun v -> not (Array.contains v ic))
    let v = if icn.Length > 0 then double icn[0] else
                if irn.Length > 0 then double (100*irn[0]) else double 0
    v


let findMirrorLine (tile: Double Matrix) =
    let ir, ic = findOld tile

    let v = if ic.Length > 0 then ic[0] else 0
    if ir.Length > 0 then 100*ir[0] + v else v

let findMirrorLine2 (tile: Double Matrix) =
    let ir, ic = findOld tile
    tile |> Matrix.mapi (findAlternateMirrorLine tile (ir, ic)) 
        |> Matrix.toSeq 
        |> Seq.filter ((<) 0)
        |> Seq.head

let solvePart1 (input: string array) =
    input |> Array.map (toGrid >> findMirrorLine) |> Seq.sum

let solvePart2 (input) =
    input |> Array.map (toGrid >> findMirrorLine2) |> Seq.sum |> int

let solve =
    let input = slurpInput "day13.txt"
                |> (fun s -> s.TrimEnd('\n').TrimEnd('\r').Split("\r\n\r\n"))

    // Solve Part 1
    let resultPart1 = solvePart1 input
    printfn "Part 1 Result: %A" resultPart1

    // Solve Part 2
    let resultPart2 = solvePart2 input
    printfn "Part 2 Result: %A" resultPart2

