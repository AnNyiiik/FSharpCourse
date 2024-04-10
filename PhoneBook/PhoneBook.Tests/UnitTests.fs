module PhoneBookTests

open System.IO
open FsUnit
open NUnit.Framework
open PhoneBook.PhoneBook

let data  =
    [
        new Person("Tom", "89019892909");
        new Person("Alice", "83890198101");
        new Person("John", "82839281789");
        new Person("Bob", "+72898271890");
        new Person("Hanna", "+71289019813");
        new Person("Sara", "81278918789")
    ]

let dataString = "Tom 89019892909
Alice 83890198101
John 82839281789
Bob +72898271890
Hanna +71289019813
Sara 81278918789"

let comparePersons (p1 : Person)  (p2 : Person) =
    if p1.Name < p2.Name then -1 else
    if p1.Name = p2.Name then
        if p1.Number < p2.Number then -1
        else 1
    else 1

[<Test>]
let ``test add record basic`` () =
    let person = new Person("Julia", "+79110606382") 
    let newData = addRecord person data
    let correctResult = List.sortWith comparePersons [ new Person("Tom", "89019892909");
        new Person("Alice", "83890198101");
        new Person("John", "82839281789");
        new Person("Bob", "+72898271890");
        new Person("Hanna", "+71289019813");
        new Person("Sara", "81278918789");
        new Person("Julia", "+79110606382")]
    let actual = List.sortWith comparePersons newData
    Assert.That(actual, Is.EqualTo(correctResult))

[<Test>]
let ``test find name by phone`` () =
    findByPhone "82839281789" data |> should equal "John"
    findByPhone "82839281780" data |> should equal "there is no such phone"

[<Test>]
let ``test find name by name`` () =
    findByName "Sara" data |> should equal "81278918789"
    findByName "Kat" data |> should equal "there is no such name"

[<Test>]
let ``test read from file`` () =
    let dataEmpty = []
    fill dataString dataEmpty |> List.sortWith comparePersons |> should
        equal
        (List.sortWith comparePersons data)