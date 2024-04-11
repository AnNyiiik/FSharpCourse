namespace PointFree

module PointFree =
    let func x l = List.map (fun y -> y * x) l
    let funcWithoutL x = List.map (fun y -> y * x)
    let funcWithoutL2 x =
        List.map ( (*) x << id )
    let funcWithoutLX : int -> int list -> int list =
        let f (x : int) = (*) x
        let f_withoutX = f
        let func  =  f_withoutX >> List.map
        func
    
         