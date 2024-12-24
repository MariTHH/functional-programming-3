module Interpolation

open System
open System.IO

type Point = float * float

let calculateLinearPoint (x0: float) (y0: float) (x1: float) (y1: float) (x: float) =
    y0 * (x1 - x) / (x1 - x0) + y1 * (x - x0) / (x1 - x0)

let linearInterpolation (points: seq<Point>) (step: float) =
    let rec interpolate currentStep remainingPoints acc =
        match Seq.length remainingPoints with
        | 1 -> Seq.append acc remainingPoints  
        | _ ->
            let (x0, y0), (x1, y1) = Seq.head remainingPoints, Seq.head (Seq.tail remainingPoints)
            if currentStep < x1 then
                let newY = calculateLinearPoint x0 y0 x1 y1 currentStep
                let resultSeq = seq { yield (currentStep, newY) }
                interpolate (currentStep + step) remainingPoints (Seq.append acc resultSeq)
            else
                interpolate (currentStep + step) (Seq.tail remainingPoints) acc
    interpolate (fst (Seq.head points)) points Seq.empty
    
let lagrangeInterpolationPoint (points: seq<Point>) (x: float) =
    points
    |> Seq.map (fun (xi, yi) ->
        let li =
            points
            |> Seq.filter (fun (xj, _) -> xi <> xj)
            |> Seq.fold (fun acc (xj, _) -> acc * (x - xj) / (xi - xj)) 1.0
        yi * li)
    |> Seq.sum

let lagrangeInterpolation (points: seq<Point>) (step: float) =
    let rec interpolate currentX acc =
        if currentX <= (Seq.last points |> fst) + step then
            let y = lagrangeInterpolationPoint points currentX
            interpolate (currentX + step) (Seq.append acc (seq { yield (currentX, y) }))
        else
            acc
    interpolate (fst (Seq.head points)) Seq.empty

let updatePoints (methodName: string) (currentPoints: seq<Point>) (newPoint: Point) =
    if methodName = "Linear" || Seq.length currentPoints >= 4 then
            Seq.append (Seq.skip 1 currentPoints) (Seq.singleton newPoint)
        else
            Seq.append currentPoints (Seq.singleton newPoint)

