module TransportTycoon.Tests.Exercise2Tests

open System
open NUnit.Framework

open TransportTycoon

let dummyLog = fun _ -> ()

[<TestCase("A", 9)>]
[<TestCase("AA", 9)>]
[<TestCase("AAA", 11)>]
[<TestCase("AAAA", 11)>]
[<TestCase("AAAAA", 25)>]
[<TestCase("AAAAAA", 25)>]
[<TestCase("AAAAAAA", 25)>]
[<TestCase("AAAAAAAA", 25)>]
[<TestCase("B", 5)>]
[<TestCase("BB", 5)>]
[<TestCase("AB", 9)>]
[<TestCase("ABB", 9)>]
[<TestCase("ABBB", 15)>]
[<TestCase("ABBBB", 17)>]
let ``Exercise 2 sample cargo lists give correct answers`` input expected =
    let result = input |> Program.parseDestinations |> Exercise2.calculateHours dummyLog
    Assert.AreEqual(expected, result)
