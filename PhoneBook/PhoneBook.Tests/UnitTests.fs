module PhoneBookTests

open System.IO
open FsUnit
open NUnit.Framework
open PhoneBook.PhoneBook

let data  =
    [
        {Name = "Tom"; Number = "89019892909"};
        {Name = "Alice"; Number = "83890198101"};
        {Name = "John"; Number = "82839281789"};
        {Name = "Bob"; Number = "+72898271890"};
        {Name = "Hanna"; Number = "+71289019813"};
        {Name = "Sara"; Number = "81278918789"}
    ]

let comparePersons (p1 : Person)  (p2 : Person) =
    if p1 < p2 then -1 else
    if p1 = p2 then
        if p1 < p2 then -1
        else 1
    else 1

let dataConvertToString = " Tom 89019892909 \n Alice 83890198101 \n John 82839281789 \n Bob +72898271890 \n Hanna +71289019813 \n Sara 81278918789 \n"
[<Test>]
let ``test convert data to string`` () =
    let res = convertDataToString data
    convertDataToString data |> should equal dataConvertToString

[<Test>]
let ``test add record basic`` () =
    let person = {Name = "Julia"; Number = "+79110606382"} 
    let newData = addRecord person data
    let correctResult = List.sortWith comparePersons [{Name = "Tom"; Number = "89019892909"};
        {Name = "Alice"; Number = "83890198101"};
        {Name = "John"; Number = "82839281789"};
        {Name = "Bob"; Number = "+72898271890"};
        {Name = "Hanna"; Number = "+71289019813"};
        {Name = "Sara"; Number = "81278918789"};
        {Name = "Julia"; Number =  "+79110606382"}]
    let actual = List.sortWith comparePersons newData
    Assert.That(actual, Is.EqualTo(correctResult))

[<Test>]
let ``test find name by phone`` () =
    (findByPhone "82839281789" data).Value |> should equal "John"
    findByPhone "82839281780" data |> should equal None

[<Test>]
let ``test find name by name`` () =
    (findByName "Sara" data).Value |> should equal "81278918789"
    findByName "Kat" data |> should equal None

let dataWrite = "Tom 89019892909\nAlice 83890198101\nJohn 82839281789\nBob +72898271890\nHanna +71289019813\nSara 81278918789"
[<Test>]
let ``test read from file`` () =
    let dataEmpty = []
    fill dataWrite dataEmpty |> List.sortWith comparePersons |> should
        equal
        (List.sortWith comparePersons data)

