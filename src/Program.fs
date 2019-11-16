module TransportTycoon.Program

open System

let parseDestinations (input: string) =
    input
    |> Seq.map (fun c ->
        match c with
        | 'A' -> A
        | 'B' -> B
        | _ -> failwithf "Unknown destination: %c" c)
    |> Seq.toList    

[<EntryPoint>]
let main argv =

    match argv with
    | [| |] -> printfn "No input provided."
    | [| s |] ->
        let destinations = parseDestinations s
        let result = Exercise1.calculateHours destinations
        printfn "Input was: %s" s
        printfn "Result   : %i" result
    | _ -> printfn "Too many inputs provided."    

    0 // return an integer exit code
