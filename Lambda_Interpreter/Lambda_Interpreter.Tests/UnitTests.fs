module Lambda_Interpreter.Tests

open NUnit.Framework

open Lambda_Interpreter.lambdaInterpreter


[<Test>]
let ``Evaluate Identity Function`` () =
    let term = Abstraction ("x", Variable "x")
    let expected = term
    let result = normalize term
    Assert.AreEqual(expected, result)

[<Test>]
let ``Evaluate Constant Function`` () =
    let term = Abstraction ("x", Abstraction ("y", Variable "x"))
    let expected = term
    let result = normalize term
    Assert.AreEqual(expected, result)
[<Test>]
let ``Evaluate Application`` () =
    let term = Application (Abstraction ("x", Variable "x"), Variable "y")
    let expected = Variable "y"
    let result = normalize term
    Assert.AreEqual(expected, result)
[<Test>]
let ``Evaluate Complex Function`` () =
    let term =
        Application (
            Abstraction ("f", Application (Variable "f", Variable "x")),
            Abstraction ("x", Variable "x")
        )
    let expected = Variable "x"
    let result = normalize term
    Assert.AreEqual(expected, result)