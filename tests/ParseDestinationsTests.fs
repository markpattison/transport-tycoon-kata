module TransportTycoon.Tests.ParseDestinationsTests

open System
open NUnit.Framework

open TransportTycoon

[<Test>]
let ``destinations parsed correctly`` () =
    let input = "ABBA"
    let expected = [ A; B; B; A ]
    
    let result = Program.parseDestinations input

    Assert.AreEqual(expected, result)

[<Test>]
let ``invalid destinations throw an error`` () =
    let input = "X"

    Assert.Throws<Exception>(fun () -> Program.parseDestinations input |> ignore)
    |> ignore
