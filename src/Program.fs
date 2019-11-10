module TransportTycoon.Program

open System

[<EntryPoint>]
let main argv =

    match argv with
    | [| |] -> printfn "No input provided."
    | [| s |] ->
        let result = Algorithm.calculateHours s
        printfn "Input was: %s" s
        printfn "Result   : %i" result
    | _ -> printfn "Too many inputs provided."    

    0 // return an integer exit code
