namespace ComputerNet

open System

module Computer = 

    type Computer (macAdress : int, OS : string,
        infectionProbability : float, isDeseasedArg : bool) =

        let mutable neighbours : Computer list = []

        member val macAdress = macAdress with get

        member val OS = OS with get

        member val infectionProbability = infectionProbability with get

        member val isDeseased = isDeseasedArg with get, set

        static member random = new System.Random()

        member computer.addNeighbour (neighbour : Computer) =
            neighbours <- neighbour :: neighbours

        member computer.getDeseased () =

            let number = Computer.random.Next(0, 100)

            let isDeseased = infectionProbability |> (*) 100.0 |> int |> (-) number |> (>) 0

            computer.isDeseased <- isDeseased

        member computer.SpreadVirus () =

            List.iter (fun (neighbour : Computer) -> (neighbour.getDeseased())) neighbours

        member computer.checkIfAllNeighboursAreDeseased () =

            List.forall (fun (neigbour : Computer) ->
                (neigbour.isDeseased || neigbour.infectionProbability = 0.0)) neighbours