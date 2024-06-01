namespace HW_7.Tests

open System.Net.Http
open Moq
open NUnit.Framework
open FsUnit
open System.Net
open System
open System.Threading
open HW_7.Lazy
open HW_7.MiniCrawler

module Tests = 

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
    let ``case two links are correct`` () =
        let mockHttp = new Mock<IHttpClient>()
        mockHttp.Setup(fun c -> c.GetAsync(It.IsAny<string>())).ReturnsAsync(
            let response = new HttpResponseMessage(HttpStatusCode.OK)
            response.Content <- new StringContent("<html><a href='https://example.com/page1'>Page 1</a><a href='https://example.com/page2'>Page 2</a></html>")
            response
        ) |> ignore

    
        mockHttp.Setup(fun c -> c.GetAsync("https://example.com/page1")).ReturnsAsync(
            let response = new HttpResponseMessage(HttpStatusCode.OK)
            response.Content <- new StringContent("Page 1 content")
            response
        ) |> ignore
    
        mockHttp.Setup(fun c -> c.GetAsync("https://example.com/page2")).ReturnsAsync(
            let response = new HttpResponseMessage(HttpStatusCode.OK)
            response.Content <- new StringContent("Page 2 content")
            response
        ) |> ignore
    
    
        let result = Async.RunSynchronously (downloadAndPrintPageSize mockHttp.Object "https://example.com")
        
        match result with
        | Some sizes ->
            Assert.AreEqual(2, sizes.Length)
            Assert.Contains(("https://example.com/page1", Some 14), sizes)
            Assert.Contains(("https://example.com/page2", Some 14), sizes)
        | None -> Assert.Fail("Expected Some result")

    [<Test>]
    let ``case one link is incorrect`` () =
        let mockHttp = new Mock<IHttpClient>()
        mockHttp.Setup(fun c -> c.GetAsync(It.IsAny<string>())).ReturnsAsync(
            let response = new HttpResponseMessage(HttpStatusCode.OK)
            response.Content <- new StringContent("<html><a href='https://site.com/page_correct'>Page 1</a><a href='https://site.com/page_incorrect'>Page 2</a></html>")
            response
        ) |> ignore
    
        mockHttp.Setup(fun c -> c.GetAsync("https://site.com/page_correct")).ReturnsAsync(
            let response = new HttpResponseMessage(HttpStatusCode.OK)
            response.Content <- new StringContent("Page 1 content")
            response
        ) |> ignore
    
        mockHttp.Setup(fun c -> c.GetAsync("https://site.com/page_incorrect")).ReturnsAsync(
            let response = new HttpResponseMessage(HttpStatusCode.NotFound)
            response
        ) |> ignore
    
        let result = Async.RunSynchronously (downloadAndPrintPageSize mockHttp.Object "https://site.com")
        
        match result with
        | Some sizes ->
            Assert.AreEqual(2, sizes.Length)
            Assert.Contains(("https://site.com/page_correct", Some 14), sizes)
            Assert.Contains(("https://site.com/page_incorrect", None), sizes)
        | None -> Assert.Fail("Expected Some result")

    [<Test>]
    let ``incorrect url should throw exeption`` () =
        let mockHttp = new Mock<IHttpClient>()
        mockHttp.Setup(fun c -> c.GetAsync("https://page_incorrect")).ReturnsAsync(
            let response = new HttpResponseMessage(HttpStatusCode.NotFound)
            response.Content <- new StringContent("Page 1 content")
            response
        ) |> ignore
        (fun () -> downloadAndPrintPageSize mockHttp.Object "https://page_incorrect" |> Async.RunSynchronously |> ignore)
        |> should throw typeof<HttpRequestException> 
