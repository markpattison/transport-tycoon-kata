module TransportTycoon.Tests.Exercise2Tests

open System
open NUnit.Framework

open TransportTycoon

let dummyLog = fun _ -> ()

[<TestCase("A", 7)>] // should be 9 with loading/unloading
[<TestCase("AA", 7)>] // should be 9 with loading/unloading
[<TestCase("AAA", 9)>] // should be 11 with loading/unloading
[<TestCase("AAAA", 9)>] // should be 11 with loading/unloading
[<TestCase("AAAAA", 21)>] // should be 25 with loading/unloading
[<TestCase("AAAAAA", 21)>] // should be 25 with loading/unloading
[<TestCase("AAAAAAA", 21)>] // should be 25 with loading/unloading
[<TestCase("AAAAAAAA", 21)>] // should be 25 with loading/unloading
[<TestCase("B", 5)>]
[<TestCase("BB", 5)>]
let ``Exercise 2 sample cargo lists give correct answers`` input expected =
    let result = input |> Program.parseDestinations |> Exercise2.calculateHours dummyLog
    Assert.AreEqual(expected, result)
