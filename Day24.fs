module Day24

#nowarn "0025" "0020"
open Utils
open System

let intersectLines [|x1: int64; y1; _; dx1; dy1; _|] [|x2: int64; y2; _; dx2; dy2; _|] =
    let v1 = (x1 - x2) * (-dy2) - (y1 - y2) * (-dx2)
    let v2 = (-dx1) * (-dy2) - (-dy1) * (-dx2)

    let u1 = (x1 - x2) * (-dy1) - (y1 - y2) * (-dx1)
    // let u2 = (-dx1) * (-dy2) - (-dy1) * (-dx2)
    if v2 = 0L then None
    else 
    let t = double v1 / double v2
    let u = double u1 / double v2
    if t < 0 || u < 0 then None
    else
    Some (double x1 + t*(double dx1), double y1 + t*(double dy1))

let crossProduct (v1: double array) (v2: double array) = 
    [| v1[1] * v2[2] - v1[2] * v2[1];
       v1[2] * v2[0] - v1[0] * v2[2];
       v1[0] * v2[1] - v1[1] * v2[0] |]

let subtract (v1: double array) (v2: double array) =
    [| v1.[0] - v2.[0]; v1.[1] - v2.[1]; v1.[2] - v2.[2] |]

let dotProduct (v1: double array) (v2: double array) = 
    v1.[0] * v2.[0] + v1.[1] * v2.[1] + v1.[2] * v2.[2]

let findIntersection (plane: double array) (line: double array) =
    let p0 = plane[..2]
    let v1 = plane[3..5]
    let v2 = plane[6..]
    let p = line[..2]
    let d = line[3..]

    let n = crossProduct v1 v2
    let denom = dotProduct n d

    // Check if the line and plane are parallel
    if denom = 0.0 then
        None  // No intersection, the line is parallel to the plane
    else
        let t = dotProduct n (subtract p0 p) / denom
        let intersection = [| p.[0] + t * d.[0]; p.[1] + t * d.[1]; p.[2] + t * d.[2] |]
        Some intersection  // Intersection point in [x, y, z] format

let solvePart1 (input) =
    input |> Array.toList |> comb2 |> List.map (tup intersectLines)
        |> List.filter (fun o -> Option.isSome o && fst (o.Value) >=< (double 200000000000000L, double 400000000000000L) && snd (o.Value) >=< (double 200000000000000L, double 400000000000000L))
        |> List.length

let solvePart2 (input: double array array) =
    let l1 = input[0]
    let inputRelL1 = input |> Array.map (fun v1 -> Array.map2 (-) v1 l1)
    let plane = Array.concat [|inputRelL1[0][..2]; inputRelL1[1]|]
    
    let inters = inputRelL1[3..] |> Array.map (twice >> mapSnd (findIntersection plane)) |> Array.filter (snd >> Option.isSome)
    let (il1, Some ip1) = inters[0]
    let (il2, Some ip2) = inters[2]

    let t1 = Array.map2 (/) (subtract ip1 il1[..2]) il1[3..]
    let t2 = Array.map2 (/) (subtract ip2 il2[..2]) il2[3..]

    let dp = subtract ip2 ip1
    let dt = t2[0] - t1[0]
    let throwt = Array.map (fun coord -> coord / dt) dp
    let throwp = subtract ip2 (throwt |> Array.map ((*) t2[0]))

    let throw = Array.concat [|throwp; throwt|] 
    let throw = Array.map2 (+) l1 throw

    throw[..2] |> Array.sum |> int64

let solve =
    let input = readInput "day24.txt"
                |> Array.map (fun s -> 
                    s.Replace(" @ ", ", ").Split(", ", StringSplitOptions.RemoveEmptyEntries)
                            |> Array.map (Int64.Parse >> double))

    // Solve Part 1
    let resultPart1 = solvePart1 (input |> Array.map (Array.map int64))
    printfn "Part 1 Result: %A" resultPart1

    // Solve Part 2
    let resultPart2 = solvePart2 input
    printfn "Part 2 Result: %A" resultPart2

