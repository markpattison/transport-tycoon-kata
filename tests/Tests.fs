module TransportTycoon.Tests

open NUnit.Framework

[<TestCase("A", 5)>]
[<TestCase("AB", 5)>]
[<TestCase("BB", 5)>]
[<TestCase("ABB", 7)>]
let Test1 input expected =

    let result = Algorithm.calculateHours input

    Assert.AreEqual(expected, result)
