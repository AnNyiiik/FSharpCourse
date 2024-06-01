namespace Lambda_Interpreter
module lambdaInterpreter =

    type LambdaTerm =
        | Variable of string
        | Abstraction of string * LambdaTerm
        | Application of LambdaTerm * LambdaTerm
    
    let rec isFreeVariable (variable: string) (expression: LambdaTerm) : bool =
        match expression with
        | Variable v -> v = variable
        | Abstraction (v, body) -> 
            if v = variable then false
            else isFreeVariable variable body
        | Application (e1, e2) -> 
            (isFreeVariable variable e1) || (isFreeVariable variable e2)
    
    let rec alphaConvert (oldName: string) (newName: string) (term: LambdaTerm) : LambdaTerm =
        match term with
        | Variable v -> 
            if v = oldName then Variable newName else term
        | Abstraction (v, body) ->
            if v = oldName then 
                Abstraction (v, body) 
            else 
                let newBody = alphaConvert oldName newName body 
                Abstraction (v, newBody)
        | Application (e1, e2) ->
            let newE1 = alphaConvert oldName newName e1 
            let newE2 = alphaConvert oldName newName e2
            Application (newE1, newE2)
    
    let rec substitute (variable: string) (value: LambdaTerm) (expression: LambdaTerm) : LambdaTerm =
        match expression with
        | Variable v -> if v = variable then value else Variable v
        | Abstraction (v, body) ->
            if v = variable then
                Abstraction (v, body) 
            elif isFreeVariable v value then
                let freshName = v + "'" 
                let newBody = alphaConvert v freshName body 
                Abstraction (freshName, substitute variable value newBody)
            else 
                Abstraction (v, substitute variable value body)
        | Application (e1, e2) -> 
            Application (substitute variable value e1, substitute variable value e2)
    
    let rec betaReduceOneStep (expression: LambdaTerm) : LambdaTerm =
        match expression with
        | Application (Abstraction (v, body), arg) -> substitute v arg body
        | Application (e1, e2) -> 
            let reducedE1 = betaReduceOneStep e1
            if reducedE1 <> e1 then Application (reducedE1, e2)
            else Application (e1, betaReduceOneStep e2)
        | _ -> expression 
    
    let rec betaReduceNormal (expression: LambdaTerm) : LambdaTerm =
        let reduced = betaReduceOneStep expression
        if reduced = expression then expression else betaReduceNormal reduced