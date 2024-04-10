module BracketsChecker.Tests

open FsUnit
open NUnit.Framework
open Brackets.Brackets
//"([{[][{{}}[]]}[()((())[])]]{[]([][{}{}])})"
let testCasesTrue =
    seq {
        yield (TestCaseData("[][{}([][(){()()}])]"))
    }
[<TestCaseSource("testCasesTrue")>]
let ``Test cases true`` (sequence : string) =
    let res = checkSequence sequence
    checkSequence sequence |> should equal true