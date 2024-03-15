module EvenNumbers.Tests

open NUnit.Framework
open FsUnit
open System

let countEvenFilter list =  list |> Seq.filter (fun x -> x % 2 = 0) |> Seq.length

let countEvenMap list = list |> Seq.map (fun x -> (1 - x % 2)) |> Seq.sum

let countEvenFold list = list |> Seq.fold (fun sum element -> (+) sum <| Math.Abs (element + 1) % 2) 0

let listPositive = [1; 2; 3; 4; 5; 6; 7; 8]
let ``correct answer positive case`` = 4
let counters = [TestCaseData(countEvenFilter), TestCaseData(countEvenMap), TestCaseData(countEvenFold)]

[<Test>]
let ``Test assert work correctly map``  =
    countEvenMap listPositive |> should equal ``correct answer positive case``

[<TestCaseSource("counters")>]
let ``Test assert work correctly`` (counter) =
    counter listPositive |> should equal ``correct answer positive case``