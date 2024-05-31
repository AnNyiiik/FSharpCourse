namespace HW_7

open System.IO
open System.Net
open System.Text.RegularExpressions
open System.Threading.Tasks

module MiniCrawler =
    
    let downloadAndPrintPageSize (url: string) =

        let readPage (link : string) =
            async {
                let request = HttpWebRequest.Create(link)
                use! response = request.AsyncGetResponse()
                use reader = new StreamReader(response.GetResponseStream())
                let html = reader.ReadToEnd()
                return html
            }
            
        async {
            try
                let html = Async.RunSynchronously (readPage url)
                
                
                let linkRegex = new Regex("<a\s+href=\"(http[s]?://[^\"#]+)\"", RegexOptions.IgnoreCase)
                let links = 
                    linkRegex.Matches(html)
                    |> Seq.cast<Match>
                    |> Seq.map (fun m -> m.Groups.[1].Value)

                let! content =
                    links
                    |> Seq.map (fun link -> 
                        async {
                            try
                                let! content = readPage link 
                                return Some (String.concat " " [link; content.Length.ToString()]) 
                            with
                                | :? WebException -> return None
                        } |> Async.StartAsTask) 
                    |> Task.WhenAll 
                    |> Async.AwaitTask 

                let mutable result = ""
                let mutable counter = 0

                for content in content do
                    match content with
                    | Some string ->
                        if (counter = 0) then 
                            result <- String.concat "" [result; string]
                            counter <- 1
                        else
                            result <- String.concat "\n" [result; string]
                    | None -> ()
                return Some result
            with
            | :? WebException -> 
                raise (WebException "Incorrect url")
                return None
        }