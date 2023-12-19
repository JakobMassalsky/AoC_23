module Day19

#nowarn "0025"
open Utils
open System

let op c = match c with
            | '>' -> (>)
            | _ -> (<)

let prop c = match c with
                | 'x' -> 0
                | 'm' -> 1
                | 'a' -> 2
                | _ -> 3

let applyRule (props: int list) (rules: string list) =
    let rec loop (rest: string list) =
        match rest with
        | [guaranteed] -> guaranteed
        | curr :: rem -> 
            let pi = prop curr[0]
            let o = op curr[1]
            let num = curr |> String.filter Char.IsDigit |> Int32.Parse
            let dest = curr[(curr.IndexOf(':') + 1)..]
            if (o props[pi] num) then dest
            else loop rem
    loop rules

let solvePart1 (ruleMap: Map<string,string list>) (parts: int list array) =
    let res p = iterate (fun s -> applyRule p ruleMap[s]) "in"
    parts |> Array.map (twice >> mapSnd (res >> Seq.find (fun key -> key = "A" || key = "R")))
        |> Array.sumBy (fun (l, r) -> if r = "A" then List.sum l else 0)

let rec getCombinations (ruleMap: Map<string,string list>) (cur: string list) (thresholds: (int64 * int64) list) =
    let testDest dest tr =
        match dest with
        | "A" -> tr |> List.fold (fun s (l, r) -> s * (r - l + 1L)) 1L
        | "R" -> 0L
        | _ -> getCombinations ruleMap ruleMap[dest] tr
    
    match cur with
    | [guaranteed] -> testDest guaranteed thresholds
    | curr :: rest ->
        let pi = prop curr[0]
        let o = op curr[1]
        let num = curr |> String.filter Char.IsDigit |> Int64.Parse
        let (l, r) = thresholds[pi]
        let dest = curr[(curr.IndexOf(':') + 1)..]
        match (o l num), (o r num) with
        | true, true -> testDest dest thresholds
        | false, false -> getCombinations ruleMap rest thresholds
        | true, false -> 
            getCombinations ruleMap rest (List.updateAt pi (num, r) thresholds) +
            testDest dest (List.updateAt pi (l, num-1L) thresholds)
        | false, true ->
            getCombinations ruleMap rest (List.updateAt pi (l, num) thresholds) +
            testDest dest (List.updateAt pi (num+1L, r) thresholds)

let solvePart2 (ruleMap: Map<string,string list>) (parts: int list array) =
    getCombinations ruleMap ruleMap["in"] [(1, 4000); (1, 4000); (1, 4000); (1, 4000)]

let solve =
    let (rules, parts) = readInput "day19.txt"
                            |> (fun a -> Array.splitAt (Array.findIndex ((=) "") a) a)

    let parts = parts |> Array.skip 1
                |> Array.map (fun s -> s.Split(',') 
                                    |> Array.map (fun s -> String.filter Char.IsDigit s |> Int32.Parse) 
                                    |> Array.toList)

    let ruleMap = rules 
                |> Array.map (fun s -> s.Split('{') |> a2t |> mapSnd (fun s -> s.TrimEnd('}').Split(',') |> Array.toList))
                |> Map.ofArray

    // Solve Part 1
    let resultPart1 = solvePart1 ruleMap parts
    printfn "Part 1 Result: %A" resultPart1

    // Solve Part 2
    let resultPart2 = solvePart2 ruleMap parts
    printfn "Part 2 Result: %A" resultPart2

