﻿module ProgressCheck1.Tests

open NUnit.Framework
open FsUnit
open ProgressCheck1.Fibonachi
open ProgressCheck1.PrintSquare
open ProgressCheck1.PriorityQueue

//[<Test>]
//let TestFibonachi () =
//    countSumFibEven() |> should equal 1089154

let squaresCases =
    seq{
        TestCaseData(4, "****\n*  *\n*  *\n****")
        TestCaseData(5, "*****\n*   *\n*   *\n*****")
    }
        
[<TestCaseSource("squaresCases")>]
let TestSquare (n : int, answer : string) =
    let ans = PrintSquare n
    PrintSquare n |> should equal answer

let queue = new PriorityQueue<int>(fun n1 n2 -> compare n1 n2)

[<Test>]
let TestEnqueue () = 
    queue.Enqueue(1)
    assert not queue.IsEmpty

//[<Test>]
//let ``dequeue from empty queue throws exception`` () =
//    queue.Clear()
//    queue.Dequeue() |> should throw typeof<System.Exception>

[<Test>]
let ``peek with empty queue throws exception`` () =
     queue.Clear()
     queue.Peek() |> should throw typeof<System.Exception> 

[<SetUp>]
let fillQueue () =
    queue.Clear()
    queue.Enqueue(1)
    queue.Enqueue(-1)
    queue.Enqueue(2)

[<Test>]
let TestDequeue () =
    let l = queue.Dequeue()
    queue.Dequeue() |> should equal 2

[<Test>]
let TestPeek () =
    let l = queue.Peek()
    queue.Peek() |> should equal 2