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

type Logger =
    {
        mutable LogsRev: string list
    }
    member this.Logs = List.rev this.LogsRev
    member this.Add s = this.LogsRev <- s :: this.LogsRev
    static member Create = { LogsRev = [] }

[<EntryPoint>]
let main argv =

    match argv with
    | [| |] -> printfn "No input provided."
    | [| s |] ->
        let destinations = parseDestinations s
        let logger = Logger.Create
        sprintf "# Deliver %s" s |> logger.Add

        let result = Exercise1.calculateHours logger.Add destinations
        
        logger.Logs |> List.iter (printfn "%s")
    | _ -> printfn "Too many inputs provided."    

    0 // return an integer exit code
