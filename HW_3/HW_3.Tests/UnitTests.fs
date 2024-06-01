module HW_3.Tests

open NUnit.Framework
open FsUnit
open HW_3.LambdaInterpreter

[<Test>]
let ``isFreeVariable: check free variable`` () =
    let expr = Application (Abstraction ("x", Variable "y"), Variable "z")
    isFreeVariable "x" expr |> should be False
    isFreeVariable "y" expr |> should be True
    isFreeVariable "z" expr |> should be True

[<Test>]
let ``alphaConvert`` () =
    let expr = Abstraction ("x", Application (Variable "x", Variable "y"))
    let expected = Abstraction ("z", Application (Variable "z", Variable "y"))
    alphaConvert "x" "z" expr |> should equal expected

[<Test>]
let ``check substitute`` () =
    let expr = Abstraction ("x", Application (Variable "x", Variable "y"))
    let value = Abstraction ("y", Variable "y")
    let expected = Abstraction ("x", Application (Variable "x", Abstraction ("y'", Variable "y'")))
    substitute "y" value expr |> should equal expected

[<Test>]
let ``betaReduceOneStep`` () =
    let expr = Application (
                    Abstraction ("x",
                        Application (Variable "x", Variable "y")), Variable "z")

    let expected = Application (Variable "z", Variable "y")
    betaReduceOneStep expr |> should equal expected

[<Test>]
let ``betaReduceNormal basic`` () =
    let expr = Application (
                    Abstraction ("x",
                        Abstraction ("y",
                            Application (Variable "x", Variable "y"))), Variable "z")

    let expected = Abstraction ("y", Application (Variable "z", Variable "y"))
    betaReduceNormal expr |> should equal expected

[<Test>]
let ``betaReduceNormal complex`` () =
    
    let expr = 
        Application (
            Abstraction ("x", (Abstraction("y", Application( Variable "x", Variable "y")))),
            Abstraction ("z", Variable "z"))
        
    let expected = 
        Abstraction ("y", Variable "y")
    betaReduceNormal expr |> should equal expected