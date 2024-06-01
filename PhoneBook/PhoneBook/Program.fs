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

///Parse comand code and check if the value is appropriate
let parseCommandCode () =
    printfn "enter a command code:\n"
    let mutable code = 0
    let mutable isCorrect = Int32.TryParse(Console.ReadLine(), &code)
    while ((not isCorrect || code > 6)) do
        printfn "incorrect comand, try again\n"
        isCorrect <- Int32.TryParse(Console.ReadLine(), &code)
    
    Some code

///Add record to the existed records
let rec addNewRecord data =
    printfn "Enter name & phone: \n"
    let personData = Console.ReadLine().Split()
    if not (personData.Length = 2) then
        printfn "incorrect data, should be 2 items, try again\n"
        addNewRecord data

    else if not (phoneRegex().IsMatch(personData.[1])) then
        printfn "incorrect phone number, try again\n"
        addNewRecord data
    else if not (nameRegex().IsMatch(personData.[0])) then
        printfn "incorrect name, try again\n"
        addNewRecord data
    else
        try
            let person = {Name = personData.[0]; Number = personData.[1]}
            addRecord person data
        with
        | :? ArgumentException ->
            printfn "this number already exist\n"
            data
            
///Find name by existed number
let rec findName data =
    printfn "enter the phone:\n"
    let phone = Console.ReadLine()
    if not(phoneRegex().IsMatch(phone)) then
        printfn "incorrect data, try again\n"
        findName data
    else
        match findByName phone data with
        | None -> printfn "%s" "there is no such name\n"
        | Some name -> printfn "%s" (name.ToString())

///Find phone by name, return first coinsidence
let rec findPhone data =
    printfn "enter the name:\n"
    let name = Console.ReadLine()
    if not(nameRegex().IsMatch(name)) then
        printfn "incorrect data, try again\n"
        findPhone data
    else
        match findByPhone name data with
        | None -> printfn "%s" "there is no such name\n"
        | Some name -> printfn "%s" (name.ToString())

///Write all the data to file
let writeFile data =
    let path = "dataWrite.txt"
    use fileWriter = new StreamWriter(path)
    let dataToWrite = convertDataToString data
    fileWriter.Write(dataToWrite)

///Read the data from file
let readFile data =
    let path = "dataRead.txt"
    use fileReader = new StreamReader(path)
    let readData = fileReader.ReadToEnd()
    fill readData data

///Process the continious loop of commands 
let rec runLoop data =

    match parseCommandCode() with
    | None | Some 0 -> Some data
    | Some 1 ->
        runLoop (addNewRecord data)
    | Some 2  ->
        findPhone data
        runLoop data
    | Some 3 ->
        findName data
        runLoop data
    | Some 4 ->
        printfn "%s" (convertDataToString data)
        runLoop data
    | Some 5 ->
        writeFile data
        runLoop data
    | Some 6 ->
        match (readFile data) with
        | None ->
            printf "incorrect data format in input file\n"
            None
        | Some data ->
            runLoop data
    | _ ->
        printfn "incorrect command code:\n"
        runLoop data

runLoop [] |> ignore