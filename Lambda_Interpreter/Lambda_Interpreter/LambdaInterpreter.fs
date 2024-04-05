namespace Lambda_Interpreter

module lambdaInterpreter = 
    type LambdaTerm =
        | Variable of string
        | Abstraction of string * LambdaTerm
        | Application of LambdaTerm * LambdaTerm

    type Substitution = (string * LambdaTerm) list

    let rec substitute (substitution: Substitution) (term: LambdaTerm) : LambdaTerm =
        match term with
        | Variable name ->
            match List.tryFind (fun (x, _) -> x = name) substitution with
            | Some (_, value) -> value
            | None -> term
        | Abstraction (arg, body) -> Abstraction (arg, substitute substitution body)
        | Application (func, arg) -> Application (substitute substitution func, substitute substitution arg)

    let rec alphaConvert (term: LambdaTerm) (boundVariables: string list) : LambdaTerm =
        match term with
        | Variable name ->
            if List.contains name boundVariables then
                let newName = name + "'"
                Variable newName
            else
                term
        | Abstraction (arg, body) ->
            let newArg =
                if List.contains arg boundVariables then
                    let newArg = arg + "'"
                    newArg
                else
                    arg
            let newBoundVariables = newArg :: boundVariables
            Abstraction (newArg, alphaConvert body newBoundVariables)
        | Application (func, arg) ->
            Application (alphaConvert func boundVariables, alphaConvert arg boundVariables)

    let rec betaReduce (term: LambdaTerm) : LambdaTerm =
        match term with
        | Variable _ -> term
        | Abstraction (arg, body) -> Abstraction (arg, betaReduce body)
        | Application (Abstraction (arg, body), argValue) ->
            let substitutedBody = substitute [(arg, argValue)] body
            betaReduce substitutedBody
        | Application (func, arg) ->
            let reducedFunc = betaReduce func
            let reducedArg = betaReduce arg
            Application (reducedFunc, reducedArg)

    let rec normalize (term: LambdaTerm) : LambdaTerm =
        let normalizedTerm = betaReduce term
        if normalizedTerm = term then
            term
        else
            normalize normalizedTerm

    let rec toString (term: LambdaTerm) : string =
        match term with
        | Variable name -> name
        | Abstraction (arg, body) -> sprintf "(λ%s.%s)" arg (toString body)
        | Application (func, arg) -> sprintf "(%s %s)" (toString func) (toString arg)