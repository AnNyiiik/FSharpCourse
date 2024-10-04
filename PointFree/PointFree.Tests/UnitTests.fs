module PointFree.Tests

open FsUnit
open FsCheck
open NUnit.Framework

let funcInitial x l = List.map (fun y -> y * x) l
let funcWithoutL1 x = List.map (fun y -> y * x)
let funcWithoutL2 x = List.map (fun y -> (*) y x)
let funcWithoutL3 x = List.map << (fun y -> (*) y) <| x
let funcWithoutLX = List.map << (fun y -> (*) y)
let funcPointFree = List.map << (*)

[<Test>]
let ``Ckeck if the funcs the same`` () =
    let isTheSame x l = funcInitial x l = funcPointFree x l
    Check.QuickThrowOnFailure isTheSame