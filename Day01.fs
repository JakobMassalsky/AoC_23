module Day01

open Utils
open System

let filterOutDigits1 (s:string) =
    let s' = String.filter Char.IsDigit s
    string s'.[0] + string s'.[s'.Length-1]

// Function for the first part of the day's problem
let filterOutDigits2 (s:string) =
    let s' = s.Replace("one", "o1e").Replace("two", "t2o").Replace("three", "t3e").Replace("four", "f4r").Replace("five", "f5e").Replace("six", "s6x").Replace("seven", "s7n").Replace("eight", "e8t").Replace("nine", "n9e")
    filterOutDigits1(s')

let sumByFilterFunction (input: string array) (filter: string -> string) =
    input |> Array.map filter
        |> Array.sumBy Int32.Parse

// Main function to run your solutions
let solve =
    let input = readInput "day01.txt" // Replace with the correct input file path

    // Solve Part 1
    let resultPart1 = sumByFilterFunction input filterOutDigits1
    printfn "Part 1 Result: %d" resultPart1

    // Solve Part 2
    let resultPart2 = sumByFilterFunction input filterOutDigits2
    printfn "Part 2 Result: %d" resultPart2

