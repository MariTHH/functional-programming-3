open System
open System.IO
open Interpolation

let computeAndPrintResults methodName points step interpolationFunc =
    if Seq.isEmpty points then
        printfn "No points provided for %s interpolation." methodName
    elif methodName = "Lagrange" && Seq.length points < 4 then
        printfn "Not enough points for %s interpolation." methodName
    else
        let results = interpolationFunc points step
        if Seq.isEmpty results then
            printfn "No results produced for %s interpolation." methodName
        else
            printfn "%s" methodName
            let formatValues = Seq.map (sprintf "%.2f")
            let xValues = results |> Seq.map fst |> formatValues |> Seq.toArray
            let yValues = results |> Seq.map snd |> formatValues |> Seq.toArray

            printfn "%s" (String.Join("    ", xValues))
            printfn "%s" (String.Join("    ", yValues))
            printfn ""

let rec interactive pointsLinear pointsLagrange step =
    computeAndPrintResults "Linear" pointsLinear step linearInterpolation
    computeAndPrintResults "Lagrange" pointsLagrange step lagrangeInterpolation

    let newPoint = 
        let parts = Console.ReadLine().Split(" ")
        (float parts.[0], float parts.[1])

    let newPointsLinear = updatePoints "Linear" pointsLinear newPoint
    let newPointsLagrange = updatePoints "Lagrange" pointsLagrange newPoint

    interactive newPointsLinear newPointsLagrange step


let handleConsoleInput () =
    printf "Step is: "
    let step = Console.ReadLine() |> float
    let firstPoint = 
        let parts = Console.ReadLine().Split(" ")
        (float parts.[0], float parts.[1])

    let secondPoint = 
        let parts = Console.ReadLine().Split(" ")
        (float parts.[0], float parts.[1])

    let initialPoints = seq { yield firstPoint; yield secondPoint }
    interactive initialPoints initialPoints step

let handleFileInput (filePath: string) =
    let lines = File.ReadAllLines(filePath)
    let step = lines.[0] |> float
    let points = lines |> Array.skip 1 |> Array.map (fun line -> 
        let parts = line.Split(" ")
        (float parts.[0], float parts.[1])) |> Seq.ofArray

    computeAndPrintResults "Linear" points step linearInterpolation
    computeAndPrintResults "Lagrange" points step lagrangeInterpolation

[<EntryPoint>]
let main args =
    match args.Length with
    | 0 -> 
        handleConsoleInput()
        0
    | 1 -> 
        handleFileInput args.[0]
        0
    | _ -> 
        printfn "Usage: [no arguments] or [path to file]."
        1 