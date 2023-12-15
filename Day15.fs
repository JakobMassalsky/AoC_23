module Day15

open Utils
open System

let singleStep i c =
    i + (int c) |> (*) 17 |> fun v -> v % 256

let applyHash (s: string) =
    s.ToCharArray() |> Array.fold singleStep 0

let solvePart1 (input) =
    input |> Array.sumBy applyHash

let minusBox label (box: (string * int) list) = 
    let i = box |> List.tryFindIndex (fst >> (=) label)
    match i with
    | Some(v) -> box |> List.removeAt v
    | None -> box

let equalBox (label, fl) (box: (string * int) list) = 
    let i = box |> List.tryFindIndex (fst >> (=) label)
    match i with
    | Some(v) -> box |> List.updateAt v (label, fl)
    | None -> box @ [(label, fl)]

let updateOne boxes (lens: string) = 
    let label = String.filter Char.IsLetter lens
    let fl = String.filter Char.IsDigit lens 
    let i = applyHash label
    if lens.Contains('-') then boxes |> Array.updateAt i (minusBox label boxes[i])
    else boxes |> Array.updateAt i (equalBox (label, fl |> Int32.Parse) boxes[i])

let sumBox (i, (box: (string * int) list)) =
    box |> List.map snd |> List.indexed |> List.sumBy (fun (ib, v) -> (ib+1)*(i+1)*v)

let solvePart2 (input) =
    let boxes = Array.init 256 (fun _ -> [])
    input |> Array.fold updateOne boxes |> Array.indexed |> Array.sumBy sumBox

let solve =
    let input = slurpInput "day15.txt" |> fun s -> s.Split(',')

    // Solve Part 1
    let resultPart1 = solvePart1 input
    printfn "Part 1 Result: %A" resultPart1

    // Solve Part 2
    let resultPart2 = solvePart2 input
    printfn "Part 2 Result: %A" resultPart2

