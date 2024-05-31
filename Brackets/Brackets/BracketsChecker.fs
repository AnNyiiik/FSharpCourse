namespace Brackets

module Brackets =

    let bracketPairs = Map['(', ')'; '{', '}'; '[', ']']

    let checkSequence (sequence : string) =
        let rec checkRec sequence stack position counter =
            if (position = String.length sequence) then
                if (counter = 0) then true else false
            else
                let bracket = sequence.[position]
                match bracket with
                | '[' | '{' | '(' ->
                    checkRec sequence (bracket :: stack) (position + 1) (counter + 1)
                | ']' | '}' | ')' ->
                    if (counter = 0) then false
                    else
                        if (bracket = bracketPairs.[stack.[0]])  then
                            checkRec sequence stack.[1..(counter - 1)] (position + 1) (counter - 1)
                        else false
                | _ -> checkRec sequence stack (position + 1) counter
        checkRec sequence []  0 0