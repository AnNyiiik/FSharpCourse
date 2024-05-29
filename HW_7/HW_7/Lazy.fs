namespace HW_7

open System.Threading

module Lazy =

   type ILazy<'a> =
        abstract member Get: unit -> 'a

    type LazySingleThread<'a>(supplier: unit -> 'a) =
        let mutable result = None

        interface ILazy<'a> with
            member this.Get() =
                match result with
                | Some x -> x
                | None ->
                    let value = supplier()
                    result <- Some (value)
                    value

    type LazyWithLock<'a>(supplier: unit -> 'a) =
        let mutable result = None
        let lockObject = obj()

        interface ILazy<'a> with
            member this.Get() =
                lock lockObject (fun () ->
                    match result with
                    | Some x -> x
                    | None ->
                        let value = supplier()
                        result <- Some (value)
                        value
                )

    type LazyLockFree<'a>(supplier: unit -> 'a) =
        let mutable result = None

        interface ILazy<'a> with
            member this.Get() =
                match System.Threading.Interlocked.CompareExchange(&result, Some(supplier()), None) with
                | Some value -> value
                | None -> result.Value