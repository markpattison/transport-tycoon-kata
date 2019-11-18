module TransportTycoon.Tests.SplitFirstMatchTests

open NUnit.Framework

open TransportTycoon

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
