module Lambda_Interpreter.Tests

open NUnit.Framework
open FsUnit
open Lambda_Interpreter.lambdaInterpreter

//[<Test>]
//let ``isFreeVariable: Проверка на свободную переменную`` () =
//    let expr = Application (Abstraction ("x", Variable "y"), Variable "z")
//    isFreeVariable "x" expr |> should be False
//    isFreeVariable "y" expr |> should be True
//    isFreeVariable "z" expr |> should be True

[<Test>]
let ``alphaConvert: Альфа-конверсия с захватом`` () =
    let expr = Abstraction ("x", Application (Variable "x", Variable "y"))
    let expected = Abstraction ("z", Application (Variable "z", Variable "y"))
    alphaConvert "x" "z" expr |> should equal expected

//[<Test>]
//let ``substitute: Подстановка с захватом`` () =
//    let expr = Abstraction ("x", Application (Variable "x", Variable "y"))
//    let value = Abstraction ("y", Variable "y")
//    let expected = Abstraction ("x'", Application (Variable "x'", Abstraction ("y'", Variable "y'")))
//    substitute "y" value expr |> should equal expected

//[<Test>]
//let ``betaReduceOneStep: Один шаг бета-редукции`` () =
//    let expr = Application (
//                    Abstraction ("x",
//                        Application (Variable "x", Variable "y")), Variable "z")

//    let expected = Application (Variable "z", Variable "y")
//    betaReduceOneStep expr |> should equal expected

//[<Test>]
//let ``betaReduceNormal: Полная бета-редукция`` () =
//    let expr = Application (
//                    Abstraction ("x",
//                        Abstraction ("y",
//                            Application (Variable "x", Variable "y"))), Variable "z")

//    let expected = Abstraction ("y", Application (Variable "z", Variable "y"))
//    betaReduceNormal expr |> should equal expected

//[<Test>]
//let ``betaReduceNormal: Редукция сложного выражения`` () =
//    // (λx. (λy. x y)) (λz. z)
//    let expr = 
//        Application (
//            Abstraction ("x", 
//                Application (
//                    Abstraction ("y", 
//                        Application (Variable "x", Variable "y")), 
//                    Variable "z")), 
//            Abstraction ("z", Variable "z"))
//    // Ожидаем: λy. (λz. z) y
//    let expected = 
//        Abstraction ("y", 
//            Application (
//                Abstraction ("z", Variable "z"),
//                Variable "y"))
//    betaReduceNormal expr |> should equal expected