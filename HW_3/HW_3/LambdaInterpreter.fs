namespace HW_3

module LambdaInterpreter =

    type LambdaTerm =
        | Variable of string
        | Abstraction of string * LambdaTerm
        | Application of LambdaTerm * LambdaTerm


    let isFreeVariable (variable: string) (expression: LambdaTerm) : bool =
        let rec isFreeVariableReq (variable: string) (bounded : string list) (expression: LambdaTerm) : bool =
            match expression with
            | Variable v -> not (List.contains variable bounded)
            | Abstraction (v, body) -> 
                if v = variable then false
                else isFreeVariableReq variable (v :: bounded) body
            | Application (e1, e2) -> 
                (isFreeVariableReq variable bounded e1) && (isFreeVariableReq variable bounded e2)
        isFreeVariableReq variable [] expression
    
    let rec alphaConvert (oldName: string) (newName: string) (term: LambdaTerm) : LambdaTerm =
        match term with
        | Variable v -> 
            if v = oldName then Variable newName else term
        | Abstraction (v, body) ->
            if v = oldName then
                if (isFreeVariable newName body) then
                    Abstraction (newName, alphaConvert oldName newName body)
                else
                    Abstraction (v, body)
            else 
                let newBody = alphaConvert oldName newName body 
                Abstraction (v, newBody)
        | Application (e1, e2) ->
            let e1Converted = alphaConvert oldName newName e1 
            let e2Converted = alphaConvert oldName newName e2
            Application (e1Converted, e2Converted)
    
    let rec substitute (variable: string) (value: LambdaTerm) (expression: LambdaTerm) : LambdaTerm =
        match expression with
        | Variable v ->
            if v = variable then
                if not (isFreeVariable v value) then
                    let newName = v + "'" 
                    let newTerm = alphaConvert v newName value
                    newTerm
                else
                    value
            else Variable v
        | Abstraction (v, body) ->
            if v = variable then
                Abstraction (v, body) 
            elif not(isFreeVariable v value) then
                let newName = v + "'" 
                let newBody = alphaConvert v newName body 
                Abstraction (newName, substitute variable value newBody)
            else 
                Abstraction (v, substitute variable value body)
        | Application (e1, e2) -> 
            Application (substitute variable value e1, substitute variable value e2)
    
    let rec betaReduceOneStep (expression: LambdaTerm) : LambdaTerm =
        match expression with
        | Application (Abstraction (v, body), arg) -> substitute v arg body
        | Application (e1, e2) -> 
            let e1Reduced = betaReduceOneStep e1
            if e1Reduced <> e1 then Application (e1Reduced, e2)
            else Application (e1, betaReduceOneStep e2)
        | Abstraction (v, body) -> 
            let reducedBody = betaReduceOneStep body
            if reducedBody <> body then
                Abstraction (v, reducedBody)
            else 
                Abstraction (v, body)
        | _ -> expression 
    
    let rec betaReduceNormal (expression: LambdaTerm) : LambdaTerm =
        let reduced = betaReduceOneStep expression
        if reduced = expression then expression else betaReduceNormal reduced