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


let mutable errors = 0
let mutable data = []

let parseCommandCode () =
    printfn "enter a command code:\n"
    let mutable code = 0
    let mutable isCorrect = Int32.TryParse(Console.ReadLine(), &code)
    while (errors < 5 && (not isCorrect || code > 6)) do
        printfn "incorrect comand, try again\n"
        isCorrect <- Int32.TryParse(Console.ReadLine(), &code)
    
    if (errors > 5) then None else
        errors <- 0
        Some code

let rec ``add record`` () =
    if (errors > 5) then ()
    printfn "Enter name & phone: \n"
    let personData = Console.ReadLine().Split()
    if not (personData.Length = 2) then
        printfn "incorrect data, should be 2 items, try again\n"
        errors <- errors + 1
        ``add record``()

    if not (phone_regex().IsMatch(personData.[1])) then
        printfn "incorrect phone number, try again\n"
        errors <- errors + 1
        ``add record``()
    if not (name_regex().IsMatch(personData.[0])) then
        printfn "incorrect name, try again\n"
        errors <- errors + 1
        ``add record``()
    else
        errors <- 0
        let person = {Name = personData.[0]; Number =  personData.[1]}
        data <- addRecord person data

let rec ``find name or phone`` code =
    if (errors > 5) then ()
    if code = 2 then printfn "enter the name:\n" else printfn "enter the phone:\n"
    let item = Console.ReadLine()
    if (code = 2 && not (name_regex().IsMatch(item)) || code = 3 && not (phone_regex().IsMatch(item))) then
        printfn "incorrect data, try again\n"
        errors <- errors + 1
        ``find name or phone`` code
    else
        errors <- 0
        if code = 2 then
            match findByName item data with
            | None -> printfn "%s" "there is no such name\n"
            | Some name -> printfn "%s" (name.ToString())
        else match findByPhone item data with
                | None -> printfn "%s" "there is no such number\n"
                | Some number -> printfn "%s" (number.ToString())

let ``write to file`` () =
    let path = "dataWrite.txt"
    use fileWriter = new StreamWriter(path)
    let dataToWrite = convertDataToString data
    fileWriter.Write(dataToWrite)

let ``read from file`` () =
    let path = "dataRead.txt"
    use fileReader = new StreamReader(path)
    let readData = fileReader.ReadToEnd()
    data <- fill readData data

let rec runLoop () =

    match parseCommandCode() with
    | None | Some 0 -> ()
    | Some 1 ->
        ``add record``()
        runLoop()
    | Some 2  ->
        ``find name or phone`` 2
        runLoop()
    | Some 3 ->
        ``find name or phone`` 3
        runLoop()
    | Some 4 ->
        printfn "%s" (convertDataToString data)
        runLoop()
    | Some 5 ->
        ``write to file`` ()
        runLoop()
    | Some 6 -> 
        ``read from file`` ()
        runLoop()
    | _ ->
        printfn "incorrect command code:\n"
        errors <- errors + 1
        runLoop()

runLoop()