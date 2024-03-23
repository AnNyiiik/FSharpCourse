namespace PrimeSequence

open System

module PrimeSequenceGenerator =
    let generatePrimes () = 
        let isPrimeNumber n =
            let bound = n |> float |> Math.Sqrt  |> Math.Round |> bigint |> (+) (bigint 1)
            let rec checkDelimeters delimeter =
                delimeter >= bound || (n % (int delimeter) <> 0 && checkDelimeters (delimeter + bigint 1))
            checkDelimeters (bigint 2)
        seq {yield! Seq.filter (fun number -> isPrimeNumber number) (Seq.initInfinite (fun i -> i + 2))}