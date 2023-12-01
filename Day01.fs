module Day01

open Utils
open System

let filterOutDigits1 (s:string) : string =
    let s' = String.filter Char.IsDigit s
    string s'.[0] + string s'.[s'.Length-1]

// Function for the first part of the day's problem
let solvePart1 (input: string array) : string =
    input |> Array.map filterOutDigits1
        |> Array.map Int32.Parse
        |> Array.sum
        |> string


open System.Text.RegularExpressions

let filterOutDigits2 (s:string) : string =
    let s' = s.Replace("one", "o1e").Replace("two", "t2o").Replace("three", "t3e").Replace("four", "f4r").Replace("five", "f5e").Replace("six", "s6x").Replace("seven", "s7n").Replace("eight", "e8t").Replace("nine", "n9e")

    let s'' = String.filter Char.IsDigit s'
    string s''.[0] + string s''.[s''.Length-1]

// Function for the second part of the day's problem
let solvePart2 (input) : string =
    input |> Array.map filterOutDigits2
        |> Array.map Int32.Parse
        |> Array.sum
        |> string

// Main function to run your solutions
let solve =
    let input = readInput "day01.txt" // Replace with the correct input file path

    // Solve Part 1
    let resultPart1 = solvePart1 input
    printfn "Part 1 Result: %s" resultPart1

    // Solve Part 2
    let resultPart2 = solvePart2 input
    printfn "Part 2 Result: %s" resultPart2

