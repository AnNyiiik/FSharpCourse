namespace HW_3

module LambdaInterpreter =

    type LambdaTerm =
        | Variable of string
        | Abstraction of string * LambdaTerm
        | Application of LambdaTerm * LambdaTerm

    /// <summary>Checks if the particular variable is contained in the expression.</summary>
    /// <param name="variable">The variable name.</param>
    ///<param name="expression">The lambda-term of an expression.</param>
    ///<returns>The truth if the variable is in the expression, otherwise false.</returns>
    let rec isVaraibleInExpression (variable: string) (expression: LambdaTerm) : bool =
        match expression with
        | Variable v -> v = variable
        | Abstraction (v, body) ->
            if v = variable then true else isVaraibleInExpression variable body
        | Application (e1, e2) ->
            (isVaraibleInExpression variable e1) || (isVaraibleInExpression variable e2)

    /// <summary>Checks if the particular variable is bounded in the expression.</summary>
    /// <param name="variable">The variable name.</param>
    ///<param name="expression">The lambda-term of an expression.</param>
    ///<returns>The truth if the variable is bounded, otherwise false.</returns>
    let isFreeVariable (variable: string) (expression: LambdaTerm) : bool =
        let rec isFreeVariableRec (variable: string) (bounded : string list) (expression: LambdaTerm): bool =
            if not (isVaraibleInExpression variable expression) then false else 
                match expression with
                | Variable v -> not (List.contains variable bounded) 
                | Abstraction (v, body) -> 
                    if v = variable then false
                    else isFreeVariableRec variable (v :: bounded) body
                | Application (e1, e2) -> 
                    (isFreeVariableRec variable bounded e1) || (isFreeVariableRec variable bounded e2)
        isFreeVariableRec variable [] expression 

    /// <summary>Performes the alpha-conversion of a given lambda-term if it's possible.</summary>
    /// <param name="oldName">The variable name which should be substituted with a new one.</param>
    ///<param name="newName">The new name of a substituted variable.</param>
    ///<returns>The lambda-term after alpha-conversion.</returns>
    let rec alphaConvert (oldName: string) (newName: string) (term: LambdaTerm) : LambdaTerm =
        match term with
        | Variable v -> 
            if v = oldName then Variable newName else term
        | Abstraction (v, body) ->
            if v = oldName then
                if not (isFreeVariable newName body) then
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

    ///<summary>Renames a variable so that new name wasn't the .</summary>
    ///<param name="oldName">The variable name which should be substituted with a new one.</param>
    ///<param name="newName">The new name of a substituted variable.</param>
    ///<returns>The lambda-term after alpha-conversion.</returns>
    let rec renameVariable (v: string) (value: LambdaTerm) (expression : LambdaTerm) : string =
        if isFreeVariable v value || isFreeVariable v expression then
            renameVariable (v + "'") value expression
        else
            v
    
    let rec substitute (variable: string) (value: LambdaTerm) (expression: LambdaTerm) : LambdaTerm =
        match expression with
        | Variable v ->
            if v = variable then value else Variable v
        | Abstraction (v, body) ->
            if v = variable then
                Abstraction (v, body) 
            elif not (isFreeVariable variable body) then
                Abstraction (v, body)
            elif isFreeVariable v value then
                let newName = renameVariable v value expression 
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