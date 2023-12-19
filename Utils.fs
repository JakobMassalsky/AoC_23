module Utils

open System
open System.IO
open MathNet.Numerics.LinearAlgebra

let inputPath = "./input/"

// Function to read input from a file
let readInput (filePath: string) : string[] =
    File.ReadAllLines(inputPath + filePath)

let slurpInput (filePath: string) : string = File.ReadAllText(inputPath + filePath) |> fun s -> s.TrimEnd('\n').TrimEnd('\r')

let inline (>=<) a (b,c) = a >= b && a <= c

let inputToCharGrid (input: string array): Char[,] = 
    let arrays = input |> Array.map Array.ofSeq
    let rows = arrays.Length
    let cols = if rows > 0 then arrays.[0].Length else 0
    Array2D.init rows cols (fun i j -> arrays.[i].[j])

let readCharGrid filePath = (readInput >> inputToCharGrid)

let inputToIntGrid (input: string array): Double Matrix = 
    let arrays = input 
                |> Array.map Array.ofSeq
    let rows = arrays.Length
    let cols = if rows > 0 then arrays.[0].Length else 0
    DenseMatrix.init rows cols (fun i j -> Double.Parse(string arrays.[i].[j]))

let readIntGrid filePath = readInput >> inputToIntGrid

let revCols m =
    m |> Matrix.toColSeq |> Seq.rev |> DenseMatrix.ofColumnSeq

let revRows m =
    m |> Matrix.toRowSeq |> Seq.rev |> DenseMatrix.ofRowSeq

let revMat m = m |> revCols |> revRows

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

let rec gcd a b = match (a,b) with
                    | (x,y) when x = y -> x
                    | (x,y) when x > y -> gcd (x-y) y
                    | (x,y) -> gcd x (y-x)

let lcm (a: int64) (b: int64) = a*b/(gcd a b)


let both f g x = (f x, g x)
let flip f a b = f b a
let swap (a,b) = (b,a)

let tup f (a, b) = f a b
let rminus a b = b - a
let rdiv a b = b / a

let clamp v mi ma = max v mi |> min ma

let rec comb n l = 
    match n, l with
    | 0, _ -> [[]]
    | _, [] -> []
    | k, (x::xs) -> List.map ((@) [x]) (comb (k-1) xs) @ comb k xs

let comb2 l = 
    l |> comb 2 |> List.map (fun v -> (v[0], v[1]))

let rec iterate f x =
        seq {
            yield x
            yield! iterate f (f x)
        }

let infinite (a:'a) = iterate id a

let until p f i = Seq.scan f i >> Seq.takeWhile (p >> not)

let tap a = printfn $"%A{a}"
            a

let inline mapSnd f (a,b) = (a,f b)
let inline mapFst f (a,b) = (f a, b)
let inline mapBoth f (a,b) = (f a, f b)
let inline twice x = (x, x)
let inline a2t (a: array<'a>) = (a[0], a[1])
let inline l2t (a: list<'a>) = (a[0], a[1])
let inline s2t (a: seq<'a>) = (Seq.item 0 a, Seq.item 1 a)

let curry f a b = f (a,b)
let uncurry f (a,b) = f a b

let foldr folder state arr = Array.foldBack folder arr state

let p a = 
    printfn "%A" a
    a

let memoize f =
    let mutable dict = Map.empty
    fun n ->
        let c = n
        match dict.ContainsKey(c) with
        | true -> dict[c]
        | false ->
            let temp = f(n)
            dict <- dict.Add((c), temp)
            temp

let memoizeh f h =
    let mutable dict = Map.empty
    fun n ->
        let c = h(n)
        match dict.ContainsKey(c) with
        | true -> dict[c]
        | false ->
            let temp = f(n)
            dict <- dict.Add(c, temp)
            temp

// Counts the number of consecutive equal elements in a list
let pack xs =
    let imp x = function
        | (i, count) :: ta when i = x -> (i, count + 1) :: ta
        | ta -> (x, 1) :: ta
    List.foldBack imp xs [] 

let applyTup f (a, b) (c, d) = (f a c, f b d)
