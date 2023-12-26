module Day22

#nowarn "0025"
open Utils
open System

let insertList a l =
    if l |> List.contains a then l else l @ [a]

let addDependency (supports: Map<int, int list>) (supportedBy: Map<int, int list>) (supporter, supported) =
    supports |> Map.add supporter (supports |> Map.tryFind supporter |> Option.defaultValue [] |> insertList supported),
    supportedBy |> Map.add supported (supportedBy |> Map.tryFind supported |> Option.defaultValue [] |> insertList supporter)

let getFallingCount (supports: Map<int, int list>) (supportedBy: Map<int, int list>) key =
    if not (supports.ContainsKey(key)) then 0
    else
    let mutable falls = Set.singleton key
    let mutable todo = supports[key]
    while todo.Length > 0 do
        let top :: rest = todo
        todo <- rest
        let bases = supportedBy[top]
        let willFall = bases |> List.exists (fun b -> not (falls.Contains(b))) |> not
        if willFall then 
            falls <- falls.Add(top)
            if supports.ContainsKey(top)
            then todo <- todo @ supports[top]
    falls.Count - 1

let solvePart1 (input: (int array * int array) array) =
    let mutable cubeGrid = Map.empty
    let mutable necessaryBricks = Map.empty
    let mutable ksupportsv = Map.empty
    let mutable ksupportedByv = Map.empty
    input |> Array.sortBy (fun (s1, _) -> s1[2]) 
        |> Array.iteri (fun ind (s, e) -> 
            if s[2] <> e[2] 
            then [0..s[2]] |> List.findBack (fun i -> cubeGrid.ContainsKey(s[0], s[1], i-1) || i = 0)
                    |> fun i ->
                        if i > 0 
                        then 
                            let a = addDependency ksupportsv ksupportedByv (cubeGrid[s[0], s[1], i-1], ind)
                            ksupportsv <- fst a
                            ksupportedByv <- snd a
                            necessaryBricks <- necessaryBricks.Add(cubeGrid[s[0], s[1], i-1], ind)
                        for z in i..(i+(e[2]-s[2])) do 
                            cubeGrid <- cubeGrid.Add((s[0], s[1], z), ind)
            else if s[1] <> e[1] 
            then [0..s[2]] |> List.findBack (fun i -> [s[1]..e[1]] |> Seq.exists (fun y -> cubeGrid.ContainsKey(s[0], y, i-1)) || i = 0)
                    |> fun i -> 
                        [s[1]..e[1]] |> List.filter (fun y -> cubeGrid.ContainsKey(s[0], y, i-1)) 
                            |> List.map (fun y -> cubeGrid[s[0], y, i-1])
                            |> List.distinct
                            |> (fun l -> 
                                if l.Length = 1 then necessaryBricks <- necessaryBricks.Add(l[0], ind); 
                                l)
                            |> List.iter (fun v ->
                                let a = addDependency ksupportsv ksupportedByv (v, ind)
                                ksupportsv <- fst a
                                ksupportedByv <- snd a)
                        for y in s[1]..e[1] do 
                            cubeGrid <- cubeGrid.Add((s[0], y, i), ind)
            else [0..s[2]] |> List.findBack (fun i -> [s[0]..e[0]] |> Seq.exists (fun x -> cubeGrid.ContainsKey(x, s[1], i-1)) || i = 0)
                    |> fun i -> 
                        [s[0]..e[0]] |> List.filter (fun x -> cubeGrid.ContainsKey(x, s[1], i-1)) 
                            |> List.map (fun x -> cubeGrid[x, s[1], i-1])
                            |> List.distinct
                            |> (fun l -> 
                                if l.Length = 1 then necessaryBricks <- necessaryBricks.Add(l[0], ind)
                                l)
                            |> List.iter (fun v -> 
                                let a = addDependency ksupportsv ksupportedByv (v, ind)
                                ksupportsv <- fst a
                                ksupportedByv <- snd a)
                        for x in s[0]..e[0] do 
                            cubeGrid <- cubeGrid.Add((x, s[1], i), ind))
    
    // cubeGrid
    // [0..input.Length-1] |> List.filter (fun i -> )
    input.Length - necessaryBricks.Count,
    [0..input.Length-1] |> List.sumBy (getFallingCount ksupportsv ksupportedByv)

let solvePart2 (input) =
    "Not implemented"

let solve =
    let input = readInput "day22.txt"
                |> Array.map (fun s -> s.Split('~') |> Array.map (fun s -> s.Split(',') |> Array.map Int32.Parse) |> a2t)

    // Solve Part 1
    let resultPart1 = solvePart1 input
    printfn "Part 1 Result: %A" resultPart1

    // Solve Part 2
    let resultPart2 = solvePart2 input
    printfn "Part 2 Result: %A" resultPart2

