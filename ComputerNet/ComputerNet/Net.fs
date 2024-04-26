namespace ComputerNet

open System 
open ComputerNet.Computer

type Net (computers : Computer list, adjacencyMatrix : int[][], OS : Map<string, int>) =

    let computers = computers

    let adjacencyMatrix = adjacencyMatrix

    do
        for i = 0 to computers.Length - 1 do
            for j = 0 to computers.Length - 1 do
                if adjacencyMatrix.[i].[j] = 1 then
                    computers.[i].addNeighbour(computers.[j]) |> ignore


    member val IsAllComputersDeseased = false with get

    member this.GetDeseasedComputers () =

        List.filter (fun (computer : Computer) -> computer.isDeseased) computers

    member this.SpreadVirus () =

        if this.IsAllComputersDeseased then true
        else
            if (List.length (this.GetDeseasedComputers()) = List.length computers) then
                true
            else
                let isAllNeigboursDeseased = true
                let deseased = this.GetDeseasedComputers()
                List.iter (fun (computer : Computer) -> computer.SpreadVirus()) deseased
                List.forall (fun (computer : Computer) -> computer.checkIfAllNeighboursAreDeseased()) deseased