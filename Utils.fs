module Utils

open System
open System.IO
open MathNet.Numerics.LinearAlgebra

let inputPath = "./input/"

// Function to read input from a file
let readInput (filePath: string) : string[] =
    File.ReadAllLines(inputPath + filePath)

let slurpInput (filePath: string) : string = File.ReadAllText(inputPath + filePath)

let inline (>=<) a (b,c) = a >= b && a<= c

let inputToCharGrid (input: string array): Char[,] = 
    let arrays = input |> Array.map Array.ofSeq
    let rows = arrays.Length
    let cols = if rows > 0 then arrays.[0].Length else 0
    Array2D.init rows cols (fun i j -> arrays.[i].[j])

let readCharGrid filePath = readInput >> inputToCharGrid

let inputToIntGrid (input: string array): int Matrix = 
    let arrays = input |> Array.map Array.ofSeq
    let rows = arrays.Length
    let cols = if rows > 0 then arrays.[0].Length else 0
    DenseMatrix.init rows cols (fun i j -> Int32.Parse(string arrays.[i].[j]))

let readIntGrid filePath = readInput >> inputToIntGrid

let boundedSlice<'T> (xmin, ymin) (xmax, ymax) (collection: 'T[,]): 'T[,] =
    collection[max 0 (ymin) .. min (Array2D.length1 collection-1) (ymax), 
               max 0 (xmin) .. min (Array2D.length2 collection-1) (xmax)]

let paddedPoint<'T> (x, y) (collection: 'T[,]): 'T[,] =
    boundedSlice (x-1, y-1) (x+1, y+1) collection

let paddedSlice<'T> (xmin, ymin) (xmax, ymax) (collection: 'T[,]) =
    boundedSlice (xmin-1, ymin-1) (xmax+1, ymax+1) collection

let inc v = v + 1
let dec v = v - 1

let stringSplit (separators: char) (options: StringSplitOptions) (str: string) = 
    str.Split(separators, options)


// type cRange =
//     struct
//         val st: int64
//         val en: int64
//         val off: int64

//         new(newv: int64, oldv: int64, len: int64) =
//             { st = oldv
//               en = oldv + len
//               off = newv - oldv }
//     end

