module BracketsChecker.Tests

open FsUnit
open NUnit.Framework
open Brackets.Brackets
let testCasesTrue =
    seq {
        yield (TestCaseData("[][{}([][(){()()}])]"))
    }

let testCasesFalse =
    seq {
        yield (TestCaseData("[]{{}}}"))
        yield (TestCaseData("[{(})]"))
        yield (TestCaseData("))(("))
        yield (TestCaseData(")"))
        yield (TestCaseData("("))
    }
[<TestCaseSource("testCasesTrue")>]
let ``Test cases true`` (sequence : string) =
    let res = checkSequence sequence
    Assert.That(checkSequence sequence)

[<TestCaseSource("testCasesFalse")>]
let ``Test cases false`` (sequence : string) =
    let res = checkSequence sequence
    Assert.That(not (checkSequence sequence))