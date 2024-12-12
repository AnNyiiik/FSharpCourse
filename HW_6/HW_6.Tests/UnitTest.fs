module HW_6.Tests

open NUnit.Framework
open FsUnit
open Calculator
open Rounder

[<Test>]
let ``test both incorrect args`` () =
    let calculator = new StringCalculatorBuilder()
    calculator {
        let! x = "a"
        let! y = "b"
        let result = x + y
        return x + y
    } |> should equal None

[<Test>]
let ``test the first arg is incorrect`` () =
    let calculator = new StringCalculatorBuilder()
    calculator {
        let! x = "a"
        let! y = "3"
        let result = x + y
        return x + y
    } |> should equal None

[<Test>]
let ```test the second arg is incorrect`` () =
    let calculator = new StringCalculatorBuilder()
    calculator {
        let! x = "3"
        let! y = "b"
        let result = x + y
        return x + y
    } |> should equal None

[<Test>]
let ```test both args are correct`` () =
    let calculator = new StringCalculatorBuilder()
    calculator {
        let! x = "3"
        let! y = "5"
        let result = x + y
        return x + y
    } |> should equal (Some 8)

[<Test>]
let ```test rounder`` () =
    let rounder = new RounderBuilder(3)
    rounder {
        let! a = 2.0 / 12.0
        let! b = 3.5
        return a / b
    } |> should equal 0.048