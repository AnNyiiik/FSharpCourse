namespace ProgressCheck1

module Fibonachi =
    
    let countSumFibEven () = 
        let rec countSum n1 n2 sum =
            match n1 with
            | (n : int) when (n >= 1000000) -> sum
            | (n : int) when (n % 2 = 0) -> countSum n2 (n2 + n1) sum
            | _ -> countSum n2 (n2 + n1) (sum + n1)
        countSum 1 1 0