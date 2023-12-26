module Hub
open System

// Main function to run the program
[<EntryPoint>]
let main args =
    if args.Length = 0 then
        printfn "Please specify a day number (e.g., 'dotnet run 01')"
        1 // Non-zero exit code to indicate an error
    else
        let day = args.[0]

        match day with
        | "01" -> Day01.solve
        | "02" -> Day02.solve
        | "03" -> Day03.solve
        | "04" -> Day04.solve
        | "05" -> Day05.solve
        | "06" -> Day06.solve
        | "07" -> Day07.solve
        | "08" -> Day08.solve
        | "09" -> Day09.solve
        | "10" -> Day10.solve
        | "11" -> Day11.solve
        | "12" -> Day12.solve
        | "13" -> Day13.solve
        | "14" -> Day14.solve
        | "15" -> Day15.solve
        | "16" -> Day16.solve
        | "17" -> Day17.solve
        | "18" -> Day18.solve
        | "19" -> Day19.solve
        | "20" -> Day20.solve
        | "21" -> Day21.solve
        | "22" -> Day22.solve
        | "23" -> Day23.solve
        | "24" -> Day24.solve
        | "25" -> Day25.solve
        | _ -> printfn "Day %s not implemented" day

        
        0 // Zero exit code to indicate success

