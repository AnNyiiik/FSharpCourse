module Calculator

open System

type StringCalculatorBuilder() =
    member this.Bind(x : string, computation) =
        match Int32.TryParse(x) with
        | true, intX -> computation intX
        | false, _ -> None

    member this.Return(x) = Some x