namespace ProgressCheck1

module PriorityQueue =

    type PriorityQueue<'T>(comparer : 'T -> 'T -> int) =

        let mutable items = []

        member this.Clear() =
            items <- []

        member this.Enqueue(item : 'T) =
            let rec insert items x = 
                match items with
                | [] -> [x]
                | y::tail when comparer x y > 0 -> x::y::tail
                | y::tail -> y::insert tail x
            items <- insert items item

        member this.Dequeue() =
            match items with
            | [] -> failwith "Queue is empty"
            | head::tail ->
                items <- tail
                head

        member this.Peek() =
            match items with
            | [] -> failwith "Queue is empty"
            | head::_ -> head

        member this.IsEmpty =
            List.length items = 0