namespace PhoneBook
open System
open System.Text.RegularExpressions

module PhoneBook =

    type Person =
        val Name : string
        val Number : string

        new (name, phone) = { Name = name; Number = phone;}

    let addRecord (person : Person) data =
        if List.exists(fun (p : Person) -> p.Number = person.Number) data
        then data else person :: data

    let findByPhone phone data =
        let result = List.tryFind(fun (p : Person) -> p.Number = phone) data

        match result with
        | Some p -> p.Name
        | None -> "there is no such phone"

    let findByName name data =
        let result = List.tryFind(fun (p : Person) -> p.Name = name) data
        
        match result with
        | Some p -> p.Number
        | None -> "there is no such name"

    let convertDataToString data =
        let rec convert data result =
            match data with
            | [] -> result
            | (head : Person) :: tail -> convert tail (String.concat " " [result; head.Name; head.Number; "\n"])
        convert data String.Empty

    let fill (personsString : string) data =

        let phone_regex = Regex(@"(\+7|7|8)+\d{10}", RegexOptions.Compiled)
        let name_regex = Regex(@"[A-Z][a-z]+", RegexOptions.Compiled)

        let persons = personsString.Split('\n')
        let rec addPersons (persons : list<string>) data =
            match persons with
            | [] -> data
            | head :: tail ->
                let personData = (head.ToString()).Split([|' '|])
                let newPerson = Person(personData.[0], personData.[1])
                if not (phone_regex.IsMatch(personData.[1]) && name_regex.IsMatch(personData.[0])) then
                    invalidArg "" "incorrect data format"
                if (personData.Length) = 2 then addPersons tail (newPerson :: data)
                    else invalidArg "" "shold be 2 values per person"
        addPersons (Seq.toList persons) data