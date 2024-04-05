namespace PhoneBook
open System

module PhoneBook =

    type Person = {Name : string; Number : string}

    let addRecord person data =
        if List.exists(fun p -> p.Number = person.Number) data
        then data else person :: data

    let findByPhone phone data =
        let result = List.tryFind(fun p -> p.Number = phone) data

        match result with
        | Some p -> Some p.Name
        | None -> None

    let findByName name data =
        let result = List.tryFind(fun p -> p.Name = name) data
        
        match result with
        | Some p -> Some p.Number
        | None -> None

    let convertDataToString data =
        let rec convert data result =
            match data with
            | [] -> result
            | head :: tail -> convert tail (String.concat " " [result; head.Name; head.Number])
        convert data String.Empty

    //let fill persons data =
    //    let rec addPersons persons data =
    //        match persons with
    //        | [] -> ()
    //        | head :: tail -> addPersons tail (({head.Name, head.Number}) :: data)
    //    addPersons persons data