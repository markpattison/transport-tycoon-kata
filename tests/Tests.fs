module TransportTycoon.Tests

open System
open NUnit.Framework

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

[<Test>]
let ``split first match works correctly when one match present`` () =
    let input = [ 1; 2; 3; 4; 5 ]
    let expected = Some (2, [ 1; 3; 4; 5 ])

    let result = Algorithm.splitFirstMatch (fun x -> x = 2) input

    Assert.AreEqual(expected, result)

[<Test>]
let ``split first match works correctly when multiple matches present`` () =
    let input = [ 1; 2; 3; 2; 4; 5; 2 ]
    let expected = Some (2, [ 1; 3; 2; 4; 5; 2 ])

    let result = Algorithm.splitFirstMatch (fun x -> x = 2) input

    Assert.AreEqual(expected, result)

[<Test>]
let ``split first match works correctly when no matches present`` () =
    let input = [ 1; 3; 4; 5 ]
    let expected = None

    let result = Algorithm.splitFirstMatch (fun x -> x = 2) input

    Assert.AreEqual(expected, result)

[<TestCase("A", 5)>]
[<TestCase("AB", 5)>]
[<TestCase("BB", 5)>]
[<TestCase("ABB", 7)>]
[<TestCase("AABABBAB", 29)>]
[<TestCase("ABBBABAAABBB", 41)>]
let ``Sample cargo lists give correct answers`` input expected =
    let result = input |> Program.parseDestinations |> Algorithm.calculateHours
    Assert.AreEqual(expected, result)
