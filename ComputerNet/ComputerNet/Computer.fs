namespace ComputerNet

open System

module Computer = 

    type Computer (macAdress : int, OS : string,
        infectionProbability : float) =

        member this.macAdress = macAdress

        member val OS = OS with get

        member val infectionProbability = infectionProbability with get

        member val isDeseased = false with get

        member val neighbours = [] with get

        static member random = new System.Random()

        member computer.addNeighbour (neighbour : Computer) =
            neighbour :: computer.neighbours

        member computer.getDeseased () =

            let number = Computer.random.Next()

            let isDeseased = infectionProbability |> (*) 100.0 |> int |> (-) number |> (>) 0

            computer.isDeseased = isDeseased

        member computer.SpreadVirus () =

            let mutable getDeseased = false

            List.iter (fun (neighbour : Computer) -> (getDeseased <- neighbour.getDeseased())) computer.neighbours

        member computer.checkIfAllNeighboursAreDeseased () =

            let mutable result = true

            List.forall (fun (neigbour : Computer) ->
                (neigbour.isDeseased || neigbour.infectionProbability = 0.0)) computer.neighbours