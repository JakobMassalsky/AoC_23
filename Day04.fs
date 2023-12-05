module Day04

open Utils
open System

let solvePart2 (input: int array) =
    input |> Array.fold (fun (tot, buff: int list) c -> 
                (tot + buff[0], (List.map ((+) buff[0]) buff[1..c]) @ buff[c+1..])) (0, List.replicate (input.Length + 10) 1)
          |> fst

let solve =
    let input = readInput "day04.txt" |> Array.map (
        (stringSplit ' ' StringSplitOptions.RemoveEmptyEntries)
        >> Array.splitAt 12
        >> (fun (s1, s2) -> 
            Set.intersect (Set.ofArray s1) (Set.ofArray s2))
        >> Set.count)

    let resultPart1 = Array.sumBy (dec >> pown 2) input
    printfn "Part 1 Result: %A" resultPart1
    
    let resultPart2 = solvePart2 input 
    printfn "Part 2 Result: %A" resultPart2
