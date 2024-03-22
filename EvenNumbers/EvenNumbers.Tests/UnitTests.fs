module EvenNumbers.Tests

open NUnit.Framework
open FsUnit
open EvenNumbers.СountEvenNumbers

let testCases =
    seq {
        yield (TestCaseData([1; 2; 3; 4; 5; 6; 7; 8], 4))
        yield (TestCaseData([-1; -2; -3; -4; -5; -6; -7; -8], 4))
    }

[<TestCaseSource("testCases")>]
let ``Test assert work correctly map`` (list : List<int>, answer : int) =
    countEvenMap list |> should equal answer

[<TestCaseSource("testCases")>]
let ``Test assert work correctly fold`` (list : List<int>, answer : int) =
    countEvenFold list |> should equal answer

[<TestCaseSource("testCases")>]
let ``Test assert work correctly filter`` (list : List<int>, answer : int) =
    countEvenFilter list |> should equal answer

[<Test>]
let ``Test empty case map`` () =
    countEvenMap [] |> should equal 0

[<Test>]
let ``Test empty case fold`` () =
    countEvenFold [] |> should equal 0

[<Test>]
let ``Test empty case filter`` () =
    countEvenFilter [] |> should equal 0