namespace EvenNumbers

open System

module СountEvenNumbers =

    let countEvenFilter list =  list |> Seq.filter (fun x -> x % 2 = 0) |> Seq.length

    let countEvenMap list = list |> Seq.map (fun x -> (1 - Math.Abs(x % 2))) |> Seq.sum

    let countEvenFold list = list |> Seq.fold (fun sum element -> (+) sum <| Math.Abs (element + 1) % 2) 0