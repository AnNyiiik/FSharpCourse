open System
open System.IO
open PhoneBook.PhoneBook

printfn "To exit press 0
To add record press 1
To find a person's phone by name press 2
To find a person's name by phone press 3
To see all saved data press 4
To save all existed data to file press 5
To read data from file and save it to phone book press 6:\n"

let parseCommandCode () =
    printfn "enter a command code:\n"
    let mutable code = 0
    let mutable isCorrect = Int32.TryParse(Console.ReadLine(), &code)
    while ((not isCorrect || code > 6)) do
        printfn "incorrect comand, try again\n"
        isCorrect <- Int32.TryParse(Console.ReadLine(), &code)
    
    Some code

let rec ``add record`` data =
    printfn "Enter name & phone: \n"
    let personData = Console.ReadLine().Split()
    if not (personData.Length = 2) then
        printfn "incorrect data, should be 2 items, try again\n"
        ``add record`` data

    else if not (phone_regex().IsMatch(personData.[1])) then
        printfn "incorrect phone number, try again\n"
        ``add record`` data
    else if not (name_regex().IsMatch(personData.[0])) then
        printfn "incorrect name, try again\n"
        ``add record`` data
    else
        try
            let person = {Name = personData.[0]; Number = personData.[1]}
            addRecord person data
        with
        | :? ArgumentException ->
            printfn "this number already exist\n"
            data
            

let rec ``find name`` data =
    printfn "enter the phone:\n"
    let phone = Console.ReadLine()
    if not(phone_regex().IsMatch(phone)) then
        printfn "incorrect data, try again\n"
        ``find name`` data
    else
        match findByName phone data with
        | None -> printfn "%s" "there is no such name\n"
        | Some name -> printfn "%s" (name.ToString())

let rec ``find phone`` data =
    printfn "enter the name:\n"
    let name = Console.ReadLine()
    if not(name_regex().IsMatch(name)) then
        printfn "incorrect data, try again\n"
        ``find phone`` data
    else
        match findByPhone name data with
        | None -> printfn "%s" "there is no such name\n"
        | Some name -> printfn "%s" (name.ToString())

let ``write to file`` data =
    let path = "dataWrite.txt"
    use fileWriter = new StreamWriter(path)
    let dataToWrite = convertDataToString data
    fileWriter.Write(dataToWrite)

let ``read from file`` data =
    let path = "dataRead.txt"
    use fileReader = new StreamReader(path)
    let readData = fileReader.ReadToEnd()
    fill readData data

let rec runLoop data =

    match parseCommandCode() with
    | None | Some 0 -> data
    | Some 1 ->
        runLoop (``add record`` data)
    | Some 2  ->
        ``find phone`` data
        runLoop data
    | Some 3 ->
        ``find name`` data
        runLoop data
    | Some 4 ->
        printfn "%s" (convertDataToString data)
        runLoop data
    | Some 5 ->
        ``write to file`` data
        runLoop data
    | Some 6 -> 
        runLoop (``read from file`` data)
    | _ ->
        printfn "incorrect command code:\n"
        runLoop data

runLoop [] |> ignore