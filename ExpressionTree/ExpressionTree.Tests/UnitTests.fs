module ExpressionTree.Tests

open NUnit.Framework
open FsUnit
open ExpressionTree

let ``test case zero division`` =
    seq {
        TestCaseData(ExpressionTree(Division, ExpressionTree( Addition, ExpressionTree (Multiplication, Leaf (3), Leaf (2)), ExpressionTree (Division, Leaf (6), Leaf (2))), Leaf (0)))
    }

[<TestCaseSource("test case zero division")>]
let ``test zero division throws division by zero exception``(tree : ExpressionTree) =
    (fun () -> countTree tree |> ignore) |> should throw typeof<System.DivideByZeroException>

let ``test cases basic`` =
    seq {
        TestCaseData(ExpressionTree(Division, Leaf(12), Leaf(6)), 2)
        TestCaseData(ExpressionTree(Multiplication, Leaf(4), Leaf(3)), 12)
        TestCaseData(ExpressionTree(Substraction, Leaf(3), Leaf(3)), 0)
        TestCaseData(ExpressionTree(Addition, Leaf(1), Leaf(0)), 1)
        TestCaseData(ExpressionTree(Addition, ExpressionTree(Multiplication, ExpressionTree(Substraction, Leaf(3), Leaf(1)), Leaf(2)), ExpressionTree(Division, Leaf(6), Leaf(3))), 6)
    }

[<TestCaseSource("test cases basic")>]
let ``test case basic`` (tree : ExpressionTree, answer : int)=
    countTree tree |> should equal answer