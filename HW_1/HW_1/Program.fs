let factorial x =
    let rec factorialReq acc x = 
        if x = 1 || x = 0 then acc 
        else 
            factorialReq 
                (acc * x)
                (x - 1)
    factorialReq 1 x

let fibonachi n =
    let rec fibonachiReq n1 n2 n = 
        if n = 0 then n1
        else if n = 1 then n2
        else
            fibonachiReq
                (n2) 
                (n1 + n2)
                (n - 1)
    fibonachiReq 0 1 n

let reverse l =
    let rec reverseReq list_new list_old = 
        if List.isEmpty list_old then list_new
        else
            let it = List.head list_old
            reverseReq (it :: list_new) (List.tail list_old)
    reverseReq [] l
    
let makeListOfPowers n m =

    let rec power n = 
        if n = 0 then 1
        else if n % 2 = 0 then 
            let a = power (n / 2)
            a * a
        else 
            let a = power (n - 1)
            2 * a

    let rec makeListOfPowersReq n i acc list = 
        if i > n then list
        else
            makeListOfPowersReq n (i + 1) (acc * 2) (acc :: list)

    let first = power n
    let result  = makeListOfPowersReq (n + m) n first []
    reverse result

let findElement x list =
    let rec findElementReq x pos list = 
        if List.length list = 0 then -1
        else if List.head list = x then pos
        else findElementReq (x) (pos + 1) (List.tail list)
    findElementReq x 0 list