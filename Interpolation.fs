module Interpolation

open System

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
