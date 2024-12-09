module Program

open System
open System.IO

type InterpolationMethod  =
    { Name: string
      Points: (float * float) seq
      Step: float
      Func: (seq<float * float> -> float -> float -> seq<float * float>) }

let lagrangeInterpolationPoint (points: seq<float * float>) (x: float) =
    points
    |> Seq.map (fun (xi, yi) ->
        let li =
            points
            |> Seq.filter (fun (xj, _) -> xi <> xj)
            |> Seq.fold (fun acc (xj, _) -> acc * (x - xj) / (xi - xj)) 1.0

        yi * li)
    |> Seq.sum

let lagrangeInterpolation (points: seq<float * float>) (startX: float) (step: float) =
    let rec interpolate currentX acc =
        if currentX <= (Seq.last points |> fst) + step then
            let y = lagrangeInterpolationPoint points currentX
            interpolate (currentX + step) (Seq.append acc (seq { yield (currentX, y) }))
        else
            acc

    interpolate startX Seq.empty

let calculateLinearPoint  (x0: float) (y0: float) (x1: float) (y1: float) (x: float) =
    y0 * (x1 - x) / (x1 - x0) + y1 * (x - x0) / (x1 - x0)

let rec interpolate xyPairs (currentStep: float) (incStep: float) =
    match Seq.length xyPairs with
    | 0 -> Seq.empty
    | _ ->
        let (x0, y0), (x1, y1) = Seq.head xyPairs
        let newY = calculateLinearPoint  x0 y0 x1 y1 currentStep
        let resultSeq = seq { yield (currentStep, newY) }

        if currentStep < x1 then
            Seq.append resultSeq (interpolate xyPairs (currentStep + incStep) incStep)
        else
            Seq.append resultSeq (interpolate (Seq.tail xyPairs) (currentStep + incStep) incStep)

let linearInterpolation xy (currentStep: float) (incStep: float) =
    let result = interpolate (Seq.pairwise xy) currentStep incStep
    let lastPoint = Seq.last xy
    Seq.append result (Seq.singleton lastPoint)

let readDataPoint () =
    let point = Console.ReadLine().Split(" ")
    let x = point[0] |> float
    let y = point[1] |> float
    (x, y)

let addPointToSequence someSeq point =
    Seq.append someSeq (Seq.singleton point)

let computeAndPrintRes approxName someSeq step approx =
    let result = approx someSeq (fst (Seq.head someSeq)) step
    printfn "%s" approxName

    let xValues = result |> Seq.map fst |> Seq.map (fun x -> sprintf "%.2f" x)
    let yValues = result |> Seq.map snd |> Seq.map (fun y -> sprintf "%.2f" y)

    let adjustedXValues =
        if approxName = "Linear" then
            Seq.take (Seq.length result - 1) xValues
        else
            xValues

    let adjustedYValues =
        if approxName = "Linear" then
            Seq.take (Seq.length result - 1) yValues
        else
            yValues

    printfn "%s" (String.Join("    ", adjustedXValues))
    printfn "%s" (String.Join("    ", adjustedYValues))
    printfn ""


let rec processInterpolation (states: seq<InterpolationMethod>) =
    for state in states do
        if state.Name = "Lagrange" && Seq.length state.Points < 4 then
            printfn "Not enough points for %s interpolation." state.Name
        else
            computeAndPrintRes state.Name state.Points state.Step state.Func

    let newPoint = readDataPoint () 

    let newStates =
        seq {
            for state in states do
                let updatedPoints =
                    if state.Name = "Linear" then
                        Seq.append (Seq.skip 1 state.Points) (Seq.singleton newPoint)
                    else if
                        Seq.length state.Points >= 4
                    then
                        Seq.append (Seq.skip 1 state.Points) (Seq.singleton newPoint)
                    else
                        Seq.append state.Points (Seq.singleton newPoint)

                yield { state with Points = updatedPoints }
        }

    processInterpolation  newStates



[<EntryPoint>]
let main (args: string[]) =
    match args.Length with
    | 0 ->
        printf "Step is: "
        let step = Console.ReadLine() |> float
        let firstPoint = readDataPoint ()
        let secondPoint = readDataPoint ()
        let someSeq = addPointToSequence (Seq.singleton firstPoint) secondPoint
        let linear = { Name = "Linear"; Points = someSeq; Step = step; Func = linearInterpolation }
        let lagrange = { Name = "Lagrange"; Points = someSeq; Step = step; Func = lagrangeInterpolation }
        processInterpolation (Seq.append (Seq.singleton linear) (Seq.singleton lagrange))
    | 1 ->
        let pointsStr = File.ReadAllLines(args[0])
        if pointsStr.Length < 1 then
            printfn "File is empty or not formatted correctly."
            1
        else
            let points =
                Seq.ofArray (
                    Array.map
                        (fun (str: string) -> 
                            let parts = str.Split(" ")
                            (float parts.[0], float parts.[1]))
                        (Array.tail pointsStr)
                )
            let step = (Array.head pointsStr) |> float
            computeAndPrintRes "Linear" points step linearInterpolation
            computeAndPrintRes "Lagrange" points step lagrangeInterpolation
            0
    | _ ->
        printfn "Usage: [no arguments] or [path to file]."
        1
