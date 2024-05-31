module HW_7.Tests

open System.Net
open NUnit.Framework
open FsUnit
open System
open System.Threading
open MiniCrawler
open Lazy

let random = Random()

let ``suppliers single thread`` =
    seq {
        TestCaseData(fun () -> (+) (random.Next(10)) <| random.Next(10))
        TestCaseData(fun () -> (*) (random.Next(10)) <| random.Next(100))
    }

let ``lazy multithread`` =
    seq {
        TestCaseData(fun supplier -> LazyWithLock (supplier) :> ILazy<obj>)
        TestCaseData(fun supplier -> LazyLockFree (supplier) :> ILazy<obj>)
    }

[<TestCaseSource("suppliers single thread")>]
let``SingleThreaded works correctly with value``(supplier : (unit -> int)) =
    let lazyItem = LazySingleThread(supplier) :> ILazy<int>

    let firstCalculation = lazyItem.Get()
    let secondCalculation = lazyItem.Get()
    let thirdCalculation = lazyItem.Get()

    firstCalculation |> should equal secondCalculation
    firstCalculation |> should equal thirdCalculation

[<TestCaseSource("lazy multithread")>]
let MultithreadTestFunction (createLazy: (unit -> 'a) -> ILazy<'a>) =

    let manualResetEvent = new ManualResetEvent(false)
    
    let counter = ref 0
    
    let supplier () =
        manualResetEvent.WaitOne() |> ignore
        Interlocked.Increment counter |> ignore
        obj ()

    manualResetEvent.Reset() |> ignore
    let lazyItem = createLazy supplier

    let tasks = Seq.init 100 (fun _ -> async { return lazyItem.Get()})
    let tasksToRun = tasks |> Async.Parallel
    manualResetEvent.Set()
    let results = tasksToRun |> Async.RunSynchronously

    let compare_item = Seq.item 0 results

    results |> Seq.forall (fun item -> obj.ReferenceEquals(item, compare_item)) |> should equal true

[<Test>]
let StandartTest () =
    downloadAndPrintPageSize "https://my.spbu.ru/"
    |> (Async.RunSynchronously)
    |> Option.get
    |> should equal "https://edu.spbu.ru/maps/map.html  107400"
    
[<Test>]
let TestWithIncorrectUrl () =
    (fun () -> downloadAndPrintPageSize "https://se.ma.su.ru/" |> Async.RunSynchronously |> ignore)
    |> should throw typeof<WebException>