open System
open System.IO
open System.Text.RegularExpressions
open PhoneBook.PhoneBook

printfn "To exit press 0\nTo add record press 1\nTo find a person's phone by name
press 2\n To find a person's name by phone press 3\nTo see all saved data press
4\nTo save all existed data to file press 5\nTo read data from file and save it
to phone book press 6:\n"

let phone_regex = Regex(@"(\+7|7|8)+\d{10}", RegexOptions.Compiled)
let name_regex = Regex(@"[A-Z][a-z]+", RegexOptions.Compiled)

let parseCommandCode () =
    printfn "enter a command code:\n"
    let code = Int32.TryParse(Console.ReadLine())
    code

let rec doLoop (commandCode : bool * int) (errors : int) (data : Person list) =

    if errors > 5 then data else

        if (not (fst commandCode)) then
            printfn "incorrect command code, should be a number, try again:\n"
            doLoop (parseCommandCode()) (errors + 1) data
        else
            try
                match (snd commandCode) with
                | 0 -> data
                | 1 ->
                    printfn "Enter name & phone: \n"
                    let personData = Console.ReadLine().Split()
                    if not (personData.Length = 2) then
                        printfn "incorrect data, should be 2 items, try again\n"
                        doLoop (true, 1) (errors + 1) data
                    else
                        if not (phone_regex.IsMatch(personData.[1])) then
                            printfn "incorrect phone number, try again\n"
                            doLoop (true, 1) (errors + 1) data
                        else
                            if not (name_regex.IsMatch(personData.[0])) then
                                printfn "incorrect name, try again\n"
                                doLoop (true, 1) (errors + 1) data
                            else
                                let person = new Person(personData.[0], personData.[1])
                                doLoop (parseCommandCode()) 0 (addRecord person data)
                | 2 ->
                    printfn "enter the name:\n"
                    let name = Console.ReadLine()
                    if not (name_regex.IsMatch(name)) then
                        printfn "incorrect name, try again\n"
                        doLoop (true, 2) (errors + 1) data
                    else
                        let result = findByName name data
                        printfn "%s" (result.ToString())
                        doLoop (parseCommandCode()) 0 data
                | 3 ->
                    printfn "enter the phone:\n"
                    let phone = Console.ReadLine()
                    if not (phone_regex.IsMatch(phone)) then
                        printfn "incorrect phone, try again\n"
                        doLoop (true, 2) (errors + 1) data
                    else
                        let result = findByPhone phone data
                        printfn "%s" (result.ToString())
                        doLoop (parseCommandCode()) 0 data
                | 4 ->
                    printfn "%s" (convertDataToString data)
                    doLoop (parseCommandCode()) 0 data
                | 5 ->
                    let path = "dataWrite.txt"
                    use fileWriter = new StreamWriter(path)
                    let dataToWrite = convertDataToString data
                    fileWriter.Write(dataToWrite)
                    doLoop (parseCommandCode()) 0 data
                | 6 ->
                    let path = "dataRead.txt"
                    use fileReader = new StreamReader(path)
                    let readData = fileReader.ReadToEnd()
                    doLoop (parseCommandCode()) 0 (fill readData data)
                | _ ->
                    printfn "%s" "incorrect option, try again"
                    doLoop (parseCommandCode()) (errors + 1) data
            with
            | invalidArg ->
                printfn "%s" "incorrect data"
                data

doLoop (parseCommandCode()) 0 []