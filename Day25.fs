module Day25

#nowarn "0025"
open Utils
open System

let insertList a l =
    if l |> List.contains a then l else l @ [a]

let addDoubleCon (graph: Map<string, string list>) left right =
    graph |> Map.add left (graph |> Map.tryFind left |> Option.defaultValue [] |> insertList right)
        |> Map.add right (graph |> Map.tryFind right |> Option.defaultValue [] |> insertList left)

let rec graphDepth (graph: Map<string, string list>) (seen: Set<string>) q =
    match q with
    // | [] -> depth
    | (next, curDepth) :: rest ->
        if seen.Contains(next) then 
            if rest.Length = 0 then curDepth else 
            graphDepth graph seen rest 
        else
        let neighbours = graph[next] |> List.filter (fun s -> Set.contains s seen |> not)
        if neighbours.Length = 0 && rest.Length = 0 then curDepth else
        graphDepth graph (Set.add next seen) (rest @ (neighbours |> List.map (fun n -> (n, curDepth+1))))

let connected (graph: Map<string, string list>) = 
    let rec graphDepth (graph: Map<string, string list>) (seen: Set<string>) q =
        match q with
        // | [] -> depth
        | next :: rest ->
            if seen.Contains(next) then 
                if rest.Length = 0 then seen else 
                graphDepth graph seen rest 
            else
            let neighbours = graph[next] |> List.filter (fun s -> Set.contains s seen |> not)
            if neighbours.Length = 0 && rest.Length = 0 then seen else
            graphDepth graph (Set.add next seen) (rest @ neighbours)
    
    graphDepth graph Set.empty [graph.Keys |> Seq.item 0] |> Set.count = graph.Count

let removeConnection (left, right) (graph: Map<string, string list>) =
    graph |> Map.add left (graph[left] |> List.removeAt (graph[left] |> List.findIndex ((=) right)))
        |> Map.add right (graph[right] |> List.removeAt (graph[right] |> List.findIndex ((=) left)))

let solvePart1 (input) =
    let rec build graph (lines: string list) =
        match lines with
        | [] -> graph
        | l :: rest -> 
            let left :: rights = l.Replace(":", "").Split(" ") |> Array.toList
            build (rights |> List.fold (fun s right -> addDoubleCon s left right) graph) rest

    let graph = build Map.empty (input |> Array.toList)
    // graphDepth graph Set.empty [("jqt", 0)]
    p (connected graph)

    let cNodes = graph.Keys |> Seq.map (twice >> mapSnd (fun node -> graphDepth graph Set.empty [(node, 0)]))
                |> Seq.sortBy snd
                |> Seq.toList
                |> List.take 10
    cNodes |> comb 3 // |> comb2 |> comb3
        |> List.map (fun nodes -> 
            nodes |> List.map fst
                // Maybe dont filter
                |> List.filter (fun node -> graph[node] |> Set.ofList |> Set.intersect (Set.ofList (nodes |> List.map fst)) |> Set.count |> (<) 0))
        |> List.filter (fun l -> l.Length = 3)
        |> List.map (fun nodes ->
            let n1 = cNodes |> List.map fst |> List.find (fun node -> node <> nodes[0] && graph[nodes[0]] |> List.contains node)
            let n2 = cNodes |> List.map fst |> List.find (fun node -> node <> nodes[1] && graph[nodes[1]] |> List.contains node)
            let n3 = cNodes |> List.map fst |> List.find (fun node -> node <> nodes[2] && graph[nodes[2]] |> List.contains node)
            [(nodes[0], n1); (nodes[1], n2); (nodes[2], n3)])
        |> List.skip 3
        |> List.map (fun [pair1; pair2; pair3] ->
            graph |> removeConnection pair1 |> removeConnection pair2 |> removeConnection pair3
            |> connected)
        
    // graph["rph"]

let solvePart2 (input) =
    "Not implemented"

let solve =
    let input = readInput "day25.txt"

    // Solve Part 1
    let resultPart1 = solvePart1 input
    printfn "Part 1 Result: %A" resultPart1

    // Solve Part 2
    let resultPart2 = solvePart2 input
    printfn "Part 2 Result: %A" resultPart2

