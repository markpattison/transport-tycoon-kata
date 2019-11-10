module TransportTycoon.Tests

open System
open NUnit.Framework

[<Test>]
let ``destinations parsed correctly`` () =
    let input = "ABBA"
    let expected = [ A; B; B; A ]
    let result = Algorithm.parseDestinations input

    Assert.AreEqual(expected, result)

[<Test>]
let ``invalid destinations throw an error`` () =
    let input = "X"

    Assert.Throws<Exception>(fun () -> Algorithm.parseDestinations input |> ignore)
    |> ignore

[<TestCase("A", 5)>]
[<TestCase("AB", 5)>]
[<TestCase("BB", 5)>]
[<TestCase("ABB", 7)>]
let ``Sample cargo lists give correct answers`` input expected =
    let result = Algorithm.calculateHours input
    Assert.AreEqual(expected, result)
