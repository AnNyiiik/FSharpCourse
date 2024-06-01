namespace PhoneBook

open System.Text.RegularExpressions

module PhoneBook =

    type Person = { Name: string; Number: string }

    let phoneRegex () = Regex(@"(\+7|7|8)+\d{10}", RegexOptions.Compiled)
    let nameRegex () = Regex(@"[A-Z][a-z]+", RegexOptions.Compiled)

    ///Add new record to the data
    let addRecord (person : Person) data =
        if List.exists(fun (p : Person) -> p.Number = person.Number) data
        then
            invalidArg "" "incorrect data: the number already exists"
            data
        else person :: data

    ///Find by phone a name of a person
    let findByPhone phone data =
        let result = List.tryFind(fun (p : Person) -> p.Number = phone) data

        match result with
        | Some p -> Some p.Name
        | None -> None

    ///Find a phone by name
    let findByName name data =
        let result = List.tryFind(fun (p : Person) -> p.Name = name) data
        
        match result with
        | Some p -> Some p.Number
        | None -> None

    ///Convert all the data to string
    let convertDataToString data =
        data 
        |> List.fold (fun acc person -> 
                            acc + $"{person.Name} {person.Number}\n") "" 

    ///Parse data from string and create list of records
    let fill (personsString : string) data =

        let persons = personsString.Split('\n')
        let rec addPersons (persons : list<string>) data =
            match persons with
            | [] -> Some data
            | head :: tail ->
                let personData = (head.ToString()).Split([|' '|])
                let newPerson = {Name = personData.[0]; Number = personData.[1]}
                if not (phoneRegex().IsMatch(personData.[1]) && nameRegex().IsMatch(personData.[0])) then
                    None
                else if (personData.Length) = 2 then addPersons tail (newPerson :: data)
                    else None
        addPersons (Seq.toList persons) data