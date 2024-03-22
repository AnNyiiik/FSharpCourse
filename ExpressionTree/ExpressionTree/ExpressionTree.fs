type Operation = Addition | Substraction | Multiplication | Division

type ExpressionTree =
    | ExpressionTree of Operation * ExpressionTree * ExpressionTree
    | Leaf of int 

let rec countTree tree = 
    match tree with 
        | Leaf n -> n
        | ExpressionTree(Addition, l, r) -> countTree l + countTree r
        | ExpressionTree(Substraction, l, r) -> countTree l - countTree r
        | ExpressionTree(Multiplication, l, r) -> countTree l * countTree r
        | ExpressionTree(Division, l, r) ->  
            let right = countTree r
            if right = 0 then raise (System.DivideByZeroException("DivisionByZero"))
            else countTree l / right