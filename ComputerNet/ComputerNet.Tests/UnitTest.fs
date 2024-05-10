module ComputerNet.Tests

open NUnit.Framework
open FsUnit
open ComputerNet.Computer
open ComputerNet.Net

let ``adjacency matrix`` =
    seq {
        TestCaseData([|
            [| 0; 0; 0; 1; 1 |]
            [| 1; 0; 0; 0; 0|]
            [| 1; 0; 0; 0; 0|]
            [| 0; 0; 1; 0; 0|]
            [| 0; 1; 0; 0; 0|]
        |]
        )
    }

let ``adjacency matrix detached case`` =
    seq {
        TestCaseData([|
            [| 0; 0; 0; 1; 0 |]
            [| 0; 0; 0; 0; 0|]
            [| 1; 0; 0; 0; 0|]
            [| 0; 0; 1; 0; 0|]
            [| 0; 1; 0; 0; 0|]
        |]
        )
    }
let ``check mac adresses`` correct result = List.map (fun (c : Computer) -> c.macAdress) result |> should equal correct

[<TestCaseSource(nameof(``adjacency matrix detached case``))>]
let ``detached computers should't be infected`` (matrix : array<array<int>>) =
    let computers = [ Computer(1, "Linux", 1.0, true);
                      Computer(2, "Linux", 1.0, false);
                      Computer(3, "Windows", 1.0, false);
                      Computer(4, "Linux", 1.0, false);
                      Computer(5, "macOS", 1.0, false)]
    let net = Net(computers, matrix)
    net.SpreadVirus() |> ignore
    net.SpreadVirus() |> ignore
    computers |> List.filter (fun (c : Computer) -> not c.isDeseased) |>
    ``check mac adresses`` [ 2; 5; ]

[<TestCaseSource(nameof(``adjacency matrix``))>]
let ``all net with probability 1 should spread virus in bfs way`` (matrix : array<array<int>>) =
    let computers = [ Computer(1, "Linux", 1.0, true);
                      Computer(2, "Linux", 1.0, false);
                      Computer(3, "Windows", 1.0, false);
                      Computer(4, "Linux", 1.0, false);
                      Computer(5, "macOS", 1.0, false)]
    let net = Net(computers, matrix)
    let infectedComputer = net.GetDeseasedComputers().[0]
    infectedComputer.SpreadVirus()
    net.GetDeseasedComputers() |> ``check mac adresses`` [ 1; 4; 5;]
    net.SpreadVirus() |> should equal true

[<TestCaseSource(nameof(``adjacency matrix``))>]
let ``net with probabilities only 0 shouldn't have any deseased computers``(matrix : array<array<int>>) =
    let computers = [ Computer(1, "Linux", 0.0, true);
                      Computer(2, "Linux", 0.0, false);
                      Computer(3, "Windows", 0.0, false);
                      Computer(4, "Linux", 0.0, false);
                      Computer(5, "macOS", 0.0, false)]
    let net = Net(computers, matrix)
    net.SpreadVirus() |> should equal true
    net.GetDeseasedComputers() |> ``check mac adresses`` [ 1; ]