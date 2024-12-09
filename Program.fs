module Program

open System
open System.IO
open Interpolation

type InterpolationMethod  =
    { Name: string
      Points: (float * float) seq
      Step: float
      Func: (seq<float * float> -> float -> float -> seq<float * float>) }

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
