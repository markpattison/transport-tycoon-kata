module TransportTycoon.Tests.Exercise1Tests

open System
open NUnit.Framework

open TransportTycoon

let dummyLog = fun _ -> ()

[<TestCase("A", 5)>]
[<TestCase("B", 5)>]
[<TestCase("AB", 5)>]
[<TestCase("BB", 5)>]
[<TestCase("ABB", 7)>]
[<TestCase("AABABBAB", 29)>]
[<TestCase("ABBBABAAABBB", 41)>]
let ``Exercise 1 sample cargo lists give correct answers`` input expected =
    let result = input |> Program.parseDestinations |> Exercise1.calculateHours dummyLog
    Assert.AreEqual(expected, result)

[<Test>]
let ``logged events are as expected`` ()=
    let input = "AB"
    let logger = Program.Logger.Create

    let expected = """{"event": "DEPART", "time": 0, "transport_id": 1, "kind": "TRUCK", "location": "FACTORY", "destination": B, "cargo": [{"cargo_id": 1, "destination": B, "origin": "FACTORY"}]}
{"event": "DEPART", "time": 0, "transport_id": 0, "kind": "TRUCK", "location": "FACTORY", "destination": PORT, "cargo": [{"cargo_id": 0, "destination": A, "origin": "FACTORY"}]}
{"event": "ARRIVE", "time": 1, "transport_id": 0, "kind": "TRUCK", "location": "PORT", "cargo": [{"cargo_id": 0, "destination": A, "origin": "FACTORY"}]}
{"event": "DEPART", "time": 1, "transport_id": 2, "kind": "SHIP", "location": "PORT", "destination": A, "cargo": [{"cargo_id": 0, "destination": A, "origin": "FACTORY"}]}
{"event": "DEPART", "time": 1, "transport_id": 0, "kind": "TRUCK", "location": "PORT", "destination": FACTORY}
{"event": "ARRIVE", "time": 2, "transport_id": 0, "kind": "TRUCK", "location": "FACTORY"}
{"event": "ARRIVE", "time": 5, "transport_id": 2, "kind": "SHIP", "location": "A", "cargo": [{"cargo_id": 0, "destination": A, "origin": "FACTORY"}]}
{"event": "ARRIVE", "time": 5, "transport_id": 1, "kind": "TRUCK", "location": "B", "cargo": [{"cargo_id": 1, "destination": B, "origin": "FACTORY"}]}
{"event": "DEPART", "time": 5, "transport_id": 2, "kind": "SHIP", "location": "A", "destination": PORT}
{"event": "DEPART", "time": 5, "transport_id": 1, "kind": "TRUCK", "location": "B", "destination": FACTORY}"""

    input |> Program.parseDestinations |> Exercise1.calculateHours logger.Add |> ignore
    let logResult = String.Join("\n", logger.Logs)

    Assert.AreEqual(expected, logResult)
