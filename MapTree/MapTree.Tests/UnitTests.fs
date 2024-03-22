module MapTree.Tests

open NUnit.Framework
open FsUnit
open MapTree.MapTree

let ``test case 1`` =
    seq {
        TestCaseData(Tree(4, Tree(2, Leaf(1), Leaf(3)), Tree(6, Leaf(5), Leaf(7))), Tree(5, Tree(3, Leaf(2), Leaf(4)), Tree(7, Leaf(6), Leaf(8))))
    }

[<TestCaseSource("test case 1")>]
let ``test 1`` (tree : Tree<int>, answer : Tree<int>) =
    mapTree tree (fun x -> x + 1) |> should equal answer

let ``test case 2`` =
    seq {
        TestCaseData(Tree("a", Tree("b", Leaf("c"), Leaf("d")), Tree("e", Leaf("f"), Leaf("g"))), Tree("aa", Tree("bb", Leaf("cc"), Leaf("dd")), Tree("ee", Leaf("ff"), Leaf("gg"))))
    }

[<TestCaseSource("test case 2")>]
let ``test 2`` (tree : Tree<string>, answer : Tree<string>) =
    mapTree tree (fun x ->  x + x) |> should equal answer