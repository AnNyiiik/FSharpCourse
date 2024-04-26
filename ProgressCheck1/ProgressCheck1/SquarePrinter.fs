namespace ProgressCheck1

module PrintSquare =

    let PrintSquare n =

        let rec makeInnerString str iter n =
            match iter with
            | (p : int) when (p = n) -> str
            | (p : int) when (p = 0 || p = n - 1) ->
                makeInnerString (String.concat "" [str; "*"]) (iter + 1) n
            | _ -> makeInnerString (String.concat "" [str; " "]) (iter + 1) n

        let rec makeBoundaries str iter n =
            match iter with
            | (p : int) when (p = n) -> str
            | _ -> makeBoundaries (String.concat "" [str; "*"]) (iter + 1) n

        let boundary = makeBoundaries "" 0 n
        let inner = makeInnerString "" 0 n
        
        let rec makeSquare square iter n =
            match iter with
            | (p : int) when (p = n - 1) -> (String.concat "\n" [square; boundary])
            | _ -> makeSquare (String.concat "\n" [square; inner]) (iter + 1) n
            
        makeSquare boundary 0 n