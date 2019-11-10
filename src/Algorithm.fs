namespace TransportTycoon

type Warehouse = A | B

type Place =
    | Factory
    | Port
    | Warehouse of Warehouse

module Algorithm =

    let parseDestinations (input: string) =
        input
        |> Seq.map (fun c ->
            match c with
            | 'A' -> A
            | 'B' -> B
            | _ -> failwithf "Unknown destination: %c" c)
        |> Seq.toList        

    let calculateHours (input: string) =
        let destinations = parseDestinations input
        0