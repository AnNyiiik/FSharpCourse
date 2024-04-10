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
    let t1 = correctResult.GetType()
    
    let actual = List.sortWith comparePersons newData
    let t2 = actual.GetType()
    Assert.That(actual, Is.EqualTo(correctResult))