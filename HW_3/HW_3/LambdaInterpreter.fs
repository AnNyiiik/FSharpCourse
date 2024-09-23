namespace HW_3

module LambdaInterpreter =

    ///The type which expresses the lambda-term.
    type LambdaTerm =
        | Variable of string
        | Abstraction of string * LambdaTerm
        | Application of LambdaTerm * LambdaTerm

    ///<summary>Checks if the particular variable is contained in the expression.</summary>
    ///<param name="variable">The variable name.</param>
    ///<param name="expression">The lambda-term of an expression.</param>
    ///<returns>The truth if the variable is in the expression, otherwise false.</returns>
    let rec isVariableInExpression (variable: string) (expression: LambdaTerm) : bool =
        match expression with
        | Variable v -> v = variable
        | Abstraction (v, body) ->
            if v = variable then true else isVariableInExpression variable body
        | Application (e1, e2) ->
            isVariableInExpression variable e1 || isVariableInExpression variable e2

    ///<summary>Checks if the particular variable is bounded in the expression.</summary>
    ///<param name="variable">The variable name.</param>
    ///<param name="expression">The lambda-term of an expression.</param>
    ///<returns>The truth if the variable is bounded, otherwise false.</returns>
    let isFreeVariable (variable: string) (expression: LambdaTerm) : bool =
        let rec isFreeVariableRec (variable: string) (bound : string list) (expression: LambdaTerm): bool =
            if not (isVariableInExpression variable expression) then false else 
                match expression with
                | Variable v -> not (List.contains variable bound) 
                | Abstraction (v, body) -> 
                    if v = variable then false
                    else isFreeVariableRec variable (v :: bound) body
                | Application (e1, e2) -> 
                    isFreeVariableRec variable bound e1 || isFreeVariableRec variable bound e2
        isFreeVariableRec variable [] expression 

    ///<summary>Performes the alpha-conversion of a given lambda-term if it's possible.</summary>
    ///<param name="oldName">The variable name which should be substituted with a new one.</param>
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

    ///<summary>Renames a variable in beta-reduction.</summary>
    ///<param name="v">The variable name which should be renamed.</param>
    ///<param name="value">The value of a term which substitudes the variable in expression.</param>
    ///<param name="expression">The expression in which the substitution is.</param>
    ///<returns>The new name of a variable.</returns>
    let rec renameVariable (v: string) (value: LambdaTerm) (expression : LambdaTerm) : string =
        if isFreeVariable v value || isFreeVariable v expression then
            renameVariable (v + "'") value expression
        else
            v

    ///<summary>Performes a substitution in the lambda-expression.</summary>
    ///<param name="variable">The name of a variable which should be substituted with a given term.</param>
    ///<param name="value">The value of a term which substitudes the variable in expression.</param>
    ///<param name="expression">The expression in which the substitution is.</param>
    ///<returns>The expression with a substituted variable if it's possible.</returns>
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

    ///<summary>Performs a one step of beta-reduction.</summary>
    ///<param name="expression">The expression in which the beta-reduction is.</param>
    ///<returns>The expression which is received after one step of beta-reduction of an initial expression.</returns>
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

    ///<summary>Performes a beta-reduction in lambda-term according to a normal strategy.</summary>
    ///<param name="expression">The expression in which the beta-reduction is.</param>
    ///<returns>The expression which is received after beta-reduction of an initial expression.</returns>
    let rec betaReduceNormal (expression: LambdaTerm) : LambdaTerm =
        let reduced = betaReduceOneStep expression
        if reduced = expression then expression else betaReduceNormal reduced