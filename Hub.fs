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
        // | "03" -> printfn "%s" Day03.solve()
        // | "04" -> printfn "%s" Day04.solve()
        // | "05" -> printfn "%s" Day05.solve()
        // | "06" -> printfn "%s" Day06.solve()
        // | "07" -> printfn "%s" Day07.solve()
        // | "08" -> printfn "%s" Day08.solve()
        // | "09" -> printfn "%s" Day09.solve()
        // | "10" -> printfn "%s" Day10.solve()
        // | "11" -> printfn "%s" Day11.solve()
        // | "12" -> printfn "%s" Day12.solve()
        // | "13" -> printfn "%s" Day13.solve()
        // | "14" -> printfn "%s" Day14.solve()
        // | "15" -> printfn "%s" Day15.solve()
        // | "16" -> printfn "%s" Day16.solve()
        // | "17" -> printfn "%s" Day17.solve()
        // | "18" -> printfn "%s" Day18.solve()
        // | "19" -> printfn "%s" Day19.solve()
        // | "20" -> printfn "%s" Day20.solve()
        // | "21" -> printfn "%s" Day21.solve()
        // | "22" -> printfn "%s" Day22.solve()
        // | "23" -> printfn "%s" Day23.solve()
        // | "24" -> printfn "%s" Day24.solve()
        // | "25" -> printfn "%s" Day25.solve()
        | _ -> printfn "Day %s not implemented" day
        0 // Zero exit code to indicate success

// Program end

    // <Compile Include="Day03.fs" />
    // <Compile Include="Day04.fs" />
    // <Compile Include="Day05.fs" />
    // <Compile Include="Day06.fs" />
    // <Compile Include="Day07.fs" />
    // <Compile Include="Day08.fs" />
    // <Compile Include="Day09.fs" />
    // <Compile Include="Day10.fs" />
    // <Compile Include="Day11.fs" />
    // <Compile Include="Day12.fs" />
    // <Compile Include="Day13.fs" />
    // <Compile Include="Day14.fs" />
    // <Compile Include="Day15.fs" />
    // <Compile Include="Day16.fs" />
    // <Compile Include="Day17.fs" />
    // <Compile Include="Day18.fs" />
    // <Compile Include="Day19.fs" />
    // <Compile Include="Day20.fs" />
    // <Compile Include="Day21.fs" />
    // <Compile Include="Day22.fs" />
    // <Compile Include="Day23.fs" />
    // <Compile Include="Day24.fs" />
    // <Compile Include="Day25.fs" />
