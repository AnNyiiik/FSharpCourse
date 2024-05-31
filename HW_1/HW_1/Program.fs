﻿//let factorial x =
//    let rec factorialReq acc x = 
//        if x = 1 || x = 0 then acc 
//        else 
//            factorialReq 
//                (acc * x)
//                (x - 1)
//    factorialReq 1 x

//let fibonachi n =
//    let rec fibonachiReq n1 n2 n = 
//        if n = 0 then n1
//        else if n = 1 then n2
//        else
//            fibonachiReq
//                (n2) 
//                (n1 + n2)
//                (n - 1)
//    fibonachiReq 0 1 n

let rec reverse list_new list_old = 
    if List.isEmpty list_old then list_new
    else
        let it = List.head list_old
        reverse (it :: list_new) (List.tail list_old)
    
let rec power n = 
    if n = 0 then 1
    else if n % 2 = 0 then 
        let a = power (n / 2)
        a * a
    else 
        let a = power (n - 1)
        2 * a

let rec makeListOfPowers n m i acc list = 
    if n > m then [] 
    else if i = m - n then list
    else 
        let acc = acc * 2
        makeListOfPowers 
            (n) 
            (m) 
            (i + 1) 
            (acc) 
            (acc :: list)

let first = power 3
let result = makeListOfPowers 7 6 0 first [first]
let result_reveresed = reverse [] result 
printfn "%A" result_reveresed

//let rec findElement x pos list = 
//    if List.length list = 0 then -1
//    else if List.head list = x then pos
//    else findElement (x) (pos + 1) (List.tail list)
