namespace InterpolationTests

open NUnit.Framework
open Interpolation
open Microsoft.FSharp.Collections

[<TestFixture>]
type InterpolationTests() =

    [<Test>]
    member _.``Test Lagrange Interpolation Function``() =
        let points = seq [ (1.0, 1.0); (2.0, 4.0); (3.0, 9.0) ]
        let result = lagrangeInterpolationPoint points 2.5
        Assert.AreEqual(6.25, result, 0.01)

    [<Test>]
    member _.``Test Linear Interpolation Function``() =
        let x0, y0, x1, y1 = 1.0, 1.0, 3.0, 9.0
        let result = calculateLinearPoint  x0 y0 x1 y1 2.0
        Assert.AreEqual(5.0, result, 0.01)

    [<Test>]
    member _.``Test Lagrange Interpolation Single Point``() =
        let points = seq [ (1.0, 5.0) ]
        let result = lagrangeInterpolationPoint points 1.0
        Assert.AreEqual(5.0, result, 0.01)

    [<Test>]
    member _.``Test Linear Interpolation Single Segment``() =
        let x0, y0, x1, y1 = 0.0, 0.0, 10.0, 10.0
        let result = calculateLinearPoint  x0 y0 x1 y1 5.0
        Assert.AreEqual(5.0, result, 0.01)
