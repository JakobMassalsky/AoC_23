module Day12

open Utils
open System

// Bit Variant //

let makeBitmasks (l: char array) = 
    let folder = (fun acc b -> (if b then 1 else 0) ||| (acc <<< 1))
    let m1 = l |> Array.map ((=) '#') |> Array.fold folder 0
    let m2 = l |> Array.map ((=) '?') |> Array.fold folder 0
    (m1, m2)

let makeIntArray (bitmask: int) length = 
    [|for i in 0..length-1 -> (bitmask >>> i) &&& 1|] |> Array.rev

let checkBitmasks (m1, m2) n = (n ||| m2) = (m1 ||| m2)

let checkArrangements (a: int array) length v =
    makeIntArray v length |> Array.toList |> pack |> List.filter (fst >> (=) 1) |> List.toArray |> Array.map snd = a

let solveLine (l: char array) (counts: int array) = 
    let masks = makeBitmasks l
    // p masks
    let nums = Array.init (pown 2 l.Length) id
    nums |> Array.filter (checkBitmasks masks) 
        |> Array.filter (checkArrangements counts l.Length)
        |> Array.length

let solvePart1 (masks) (counts) =
   Array.map2 solveLine masks counts |> Array.sum

// Recursive Variant //

let findPlaces (l: char array) count total =
    let positions = l.Length - total
    Array.init (positions+1) id 
        |> fun a -> 
            (Array.FindAll(a, (fun i -> l[i..(i+count-1)] |> Array.exists ((=) '.') |> not)))

let rec fillArrangement = 
    memoizeh (fun ((l: char array), (counts: int array)) ->
        if l.Length = 0 then 0L
        else 
        let total = (counts |> Array.sum) + counts.Length - 1
        if total > l.Length then 0L
        else 
        let c0 = counts[0]
        let places = findPlaces l c0 total
        if places.Length = 0 then 0L
        else
        let places = places |> Array.filter (fun p -> (p+c0 >= l.Length || l[p+c0] <> '#') && not (Array.exists ((=)'#') l[..p-1]))
        if counts.Length = 1 then places |> Array.sumBy (fun p -> if not (Array.exists ((=)'#') l[(p+c0+1)..]) then 1 else 0)
        else
        places |> Array.map (fun p -> fillArrangement ((l[(p+c0+1)..]), (Array.tail counts))) |> Array.sum)
        (fun ((l: char array), (counts: int array)) -> ((String.Join(' ', l) + String.Join(' ', Array.map (fun s -> s.ToString()) counts))))

let solvePart2 (masks) (counts) =
    Array.zip masks counts |> Array.mapi (fun i a -> fillArrangement a)

let solve =
    let input = readInput "day12.txt"
                |> Array.map (fun s -> s.Split(' '))

    let masks = input |> Array.map (Array.head >> fun s -> s.ToCharArray())// |> Array.replicate 5 |> Array.concat) //
    let counts = input |> Array.map (Array.last >> (fun s -> s.Split(',') |> Array.map Int32.Parse))// |> Array.replicate 5 |> Array.concat)) // 
    // Solve Part 1
    let resultPart1 = solvePart2 masks counts |> Array.sum
    printfn "Part 1 Result: %A" resultPart1
    
    let masks = input |> Array.map (Array.head >> fun s -> (s+"?"+s+"?"+s+"?"+s+"?"+s).ToCharArray()) //
    let counts = input |> Array.map (Array.last >> (fun s -> (s+","+s+","+s+","+s+","+s).Split(',') |> Array.map Int32.Parse))
    // Solve Part 2
    let resultPart2 = solvePart2 masks counts |> Array.sum
    printfn "Part 2 Result: %A" resultPart2

