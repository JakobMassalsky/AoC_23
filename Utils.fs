module Utils

open System
open System.IO

let inputPath = "./input/"

// Function to read input from a file
let readInput (filePath: string) : string[] =
    File.ReadAllLines(inputPath + filePath)
