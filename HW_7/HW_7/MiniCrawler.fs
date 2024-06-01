namespace HW_7

open System.Net
open System.Text.RegularExpressions
open System.Threading.Tasks
open System.Net.Http

module MiniCrawler =

    type IHttpClient =
        abstract member GetAsync : string -> Task<HttpResponseMessage>

    let readPage (client: IHttpClient) (link : string) =

        async {
           let! response = client.GetAsync(link) |> Async.AwaitTask
           response.EnsureSuccessStatusCode() |> ignore
           let! content = response.Content.ReadAsStringAsync() |> Async.AwaitTask
           return content
        }
    
    let downloadAndPrintPageSize (client: IHttpClient) (url: string) =

        async {
            try
                let html = Async.RunSynchronously  (readPage client url)
                
                
                let linkRegex = new Regex("<a\s+href=\'(http[s]?://[^\'#]+)\'", RegexOptions.IgnoreCase)
                let links = 
                    linkRegex.Matches(html)
                    |> Seq.cast<Match>
                    |> Seq.map (fun m -> m.Groups.[1].Value)

                let! content =
                    links
                    |> Seq.map (fun link -> 
                        async {
                            try
                                let! content = readPage client link 
                                return (link, Some content.Length) 
                            with
                                | :? HttpRequestException -> return (link, None)
                        }) 
                    |> Async.Parallel

                return Some content 
            with
            | :? WebException -> 
                raise (WebException "Incorrect url")
                return None
        }