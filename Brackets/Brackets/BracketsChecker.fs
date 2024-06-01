namespace Brackets

module Brackets =

    let bracketPairs = Map['(', ')'; '{', '}'; '[', ']']
    
    let checkSequence (sequence : string) =
        let rec checkRec (sequence : string) stack position counter =
            if (position = sequence.Length) then if (counter = 0) then true else false
        
            else
                let bracket = sequence.[position]
                if bracketPairs.ContainsKey bracket then
                    checkRec sequence (bracket :: stack) (position + 1) (counter + 1)
                else
                    if (bracketPairs.Values.Contains bracket) then
                        match stack with
                        | [] -> false
                        | head :: tail -> 
                            if (bracket = bracketPairs.[head]) then
                                checkRec sequence tail (position + 1) (counter - 1)
                            else false
                    else checkRec sequence stack (position + 1) counter
        checkRec sequence []  0 0