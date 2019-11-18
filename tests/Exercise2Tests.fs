module TransportTycoon.Tests.Exercise2Tests

open System
open NUnit.Framework

open TransportTycoon

let dummyLog = fun _ -> ()

//[<TestCase("A", 9)>]
[<TestCase("B", 5)>]
[<TestCase("BB", 5)>]
let ``Exercise 2 sample cargo lists give correct answers`` input expected =
    let result = input |> Program.parseDestinations |> Exercise2.calculateHours dummyLog
    Assert.AreEqual(expected, result)
