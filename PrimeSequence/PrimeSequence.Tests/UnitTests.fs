module PrimeSequence.Tests

open NUnit.Framework
open FsUnit
open PrimeSequenceGenerator

[<Test>]
let ``compare first 10 numbers`` () =
    let correctAnswer = [2; 3; 5; 7; 11; 13; 17; 19; 23; 29]
    generatePrimes () |> Seq.take 10 |> Seq.toList |> should equal correctAnswer