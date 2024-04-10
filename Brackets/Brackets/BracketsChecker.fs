namespace Brackets

module Brackets =

    let checkSequence (sequence : string) =
        let rec checkRec sequence stack position =
            if (position = String.length sequence) then true
            else
                let bracket = sequence.[position]
                match bracket with
                | '[' | '{' | '(' ->
                    checkRec sequence (String.concat "" [bracket.ToString(); stack]) (position + 1)
                | ']' | '}' | ')' ->
                    if (String.length stack = 0) then false
                    else
                        if ((bracket = ')' && stack.[0] = '(') ||
                        (bracket = ']' && stack.[0] = '[') ||
                        (bracket = '}' && stack.[0] = '{')) then
                            checkRec sequence stack.[1..(String.length stack - 1)] (position + 1)
                        else false
                | _ -> checkRec sequence stack (position + 1)
        checkRec sequence ""  0