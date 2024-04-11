module PointFree.Tests

open FsUnit
open FsCheck
open NUnit.Framework
open PointFree.PointFree

[<Test>]
let ``Ckeck if the funcs the same`` () =
    let isTheSame x l = func x l = funcWithoutLX x l
    Check.QuickThrowOnFailure isTheSame