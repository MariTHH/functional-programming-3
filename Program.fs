module Program

open System
open System.IO
open Interpolation

type Point = float * float

type InterpolationMethod =
    { Name: string
      Points: seq<Point>
      Step: float
      Func: (seq<Point> -> float -> float -> seq<Point>) }

let parsePoint (line: string) : Point =
    let parts = line.Split(" ")
    (float parts.[0], float parts.[1])

let readDataPoint () : Point =
    Console.ReadLine() |> parsePoint

let addPointToSequence (points: seq<Point>) (point: Point) =
    Seq.append points (Seq.singleton point)

let computeAndPrintResult methodName points step interpolationFunc =
    let results = interpolationFunc points (fst (Seq.head points)) step
    printfn "%s" methodName

    let formatValues = Seq.map (sprintf "%.2f")
    let xValues = results |> Seq.map fst |> formatValues
    let yValues = results |> Seq.map snd |> formatValues

    let adjustedValues values =
        if methodName = "Linear" then Seq.take (Seq.length results - 1) values else values

    printfn "%s" (String.Join("    ", adjustedValues xValues))
    printfn "%s" (String.Join("    ", adjustedValues yValues))
    printfn ""

let handleUserInterpolation (methods: seq<InterpolationMethod>) =
    let rec loop (currentMethods: seq<InterpolationMethod>) =
        currentMethods |> Seq.iter (fun method ->
            if method.Name = "Lagrange" && Seq.length method.Points < 4 then
                printfn "Not enough points for %s interpolation." method.Name
            else
                computeAndPrintResult method.Name method.Points method.Step method.Func)

        let newPoint = readDataPoint()

        let updatedMethods =
            currentMethods
            |> Seq.map (fun method ->
                let updatedPoints =
                    match method.Name with
                    | "Linear" -> Interpolation.updateLinearPoints method.Points newPoint
                    | "Lagrange" -> Interpolation.updateLagrangePoints method.Points newPoint
                    | _ -> method.Points
                { method with Points = updatedPoints })

        loop updatedMethods

    loop methods


let handleConsoleInput () =
    printf "Step is: "
    let step = Console.ReadLine() |> float
    let firstPoint = readDataPoint ()
    let secondPoint = readDataPoint ()
    let initialPoints = addPointToSequence (Seq.singleton firstPoint) secondPoint

    let linear = { Name = "Linear"; Points = initialPoints; Step = step; Func = linearInterpolation }
    let lagrange = { Name = "Lagrange"; Points = initialPoints; Step = step; Func = lagrangeInterpolation }

    handleUserInterpolation (Seq.singleton linear |> Seq.append (Seq.singleton lagrange))

let handleFileInput (filePath: string) =
    let lines = File.ReadAllLines(filePath)

    if lines.Length < 1 then
        printfn "File is empty or not formatted correctly."
        1
    else
        let step = lines.[0] |> float
        let points = lines |> Array.skip 1 |> Array.map parsePoint |> Seq.ofArray

        computeAndPrintResult "Linear" points step linearInterpolation
        computeAndPrintResult "Lagrange" points step lagrangeInterpolation
        0

[<EntryPoint>]
let main (args: string[]) =
    match args.Length with
    | 0 -> 
        handleConsoleInput()
        0
    | 1 -> 
        handleFileInput args.[0]
    | _ -> 
        printfn "Usage: [no arguments] or [path to file]."
        1