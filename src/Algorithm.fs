module TransportTycoon.Algorithm

let logging = true

let log state msg = if logging then printfn "Time %i: %s" state.Time msg

let distance location1 location2 =
    match location1, location2 with
    | Factory, Port        | Port, Factory        -> 1
    | Port, Warehouse A    | Warehouse A, Port    -> 4
    | Factory, Warehouse B | Warehouse B, Factory -> 5
    | _ -> failwith "Impossible journey"

let initialVehicles =
    [ { Type = Truck; Location = At Factory; Cargo = None }
      { Type = Truck; Location = At Factory; Cargo = None }
      { Type = Ship; Location = At Port; Cargo = None }
    ]

let initialState cargo =
    { Time = 0
      Queues = [ (Factory, cargo); (Port, []); (Warehouse A, []); (Warehouse B, []) ] |> Map.ofList
      Vehicles = initialVehicles }

let parseDestinations (input: string) =
    input
    |> Seq.map (fun c ->
        match c with
        | 'A' -> A
        | 'B' -> B
        | _ -> failwithf "Unknown destination: %c" c)
    |> Seq.toList        

let unloadVehicle v =
    match v.Cargo with
    | Some cargo -> { v with Cargo = None }, cargo
    | None -> failwith "Cannot unload empty vehicle"


let splitFirstMatch predicate lst =
    let rec split acc l =
        match l with
        | x :: xs when predicate x -> Some (x, acc @ xs)
        | x :: xs -> split (acc @ [ x ]) xs
        | [] -> None
    split [] lst

let (|EmptyVehicleAt|_|) vehicleType location state =
    match splitFirstMatch (fun v -> v.Type = vehicleType && v.Location = At location && v.Cargo.IsNone) state.Vehicles with
    | Some (emptyVehicle, otherVehicles) -> Some (emptyVehicle, otherVehicles)
    | _ -> None

let (|LoadedVehicleAt|_|) vehicleType location state =
    match splitFirstMatch (fun v -> v.Type = vehicleType && v.Location = At location && v.Cargo.IsSome) state.Vehicles with
    | Some (loadedVehicle, otherVehicles) -> Some (loadedVehicle, otherVehicles)
    | _ -> None

let (|CargoAt|_|) location state =
    match state.Queues.[location] with
    | cargoToLoad :: remainingCargo -> Some (cargoToLoad, remainingCargo)
    | [] -> None

let loadCargo vehicleType location state =
    match state with
    | EmptyVehicleAt vehicleType location (emptyVehicle, otherVehicles) & CargoAt location (cargoToLoad, remainingCargo) ->
        let loadedVehicle = { emptyVehicle with Cargo = Some cargoToLoad }
        sprintf "Loading: %O onto %O" cargoToLoad emptyVehicle |> log state
        Some { state with Queues = state.Queues.Add(location, remainingCargo); Vehicles = loadedVehicle :: otherVehicles }
    | _ -> None    

let despatch vehicleType location findDestination state =
    match state with
    | LoadedVehicleAt vehicleType location (loadedVehicle, otherVehicles) ->
        let destination = findDestination loadedVehicle.Cargo.Value
        let journey = (location, state.Time, destination, state.Time + distance location destination)
        let movingVehicle = { loadedVehicle with Location = Journey journey }
        sprintf "Despatching: %O, %O" loadedVehicle movingVehicle.Location |> log state
        Some { state with Vehicles = movingVehicle :: otherVehicles }
    | _ -> None    

let unload vehicleType location state =
    match state with
    | LoadedVehicleAt vehicleType location (loadedVehicle, otherVehicles) ->
        let unloadedVehicle, cargo = unloadVehicle loadedVehicle
        sprintf "Unloading: %O" loadedVehicle |> log state
        Some { state with Queues = state.Queues.Add(location, cargo :: state.Queues.[location]); Vehicles = unloadedVehicle :: otherVehicles }
    | _ -> None    

let returnEmpty vehicleType location destination state =
    match state with
    | EmptyVehicleAt vehicleType location (emptyVehicle, otherVehicles) ->
        let journey = (location, state.Time, destination, state.Time + distance location destination)
        let movingVehicle = { emptyVehicle with Location = Journey journey }
        sprintf "Returning: %O, %O" emptyVehicle movingVehicle.Location |> log state
        Some { state with Vehicles = movingVehicle :: otherVehicles }
    | _ -> None    

let arriveAll state =
    let updatedVehicles =
        state.Vehicles
        |> List.map (fun v ->
            match v.Location with
            | Journey (_, _, location, t) when t = state.Time ->
                sprintf "Arriving: %O" v |> log state
                { v with Location = At location }
            | _ -> v)
    { state with Vehicles = updatedVehicles }

let timePasses state =
    let tNext = state.Time + 1
    if tNext > 50 then failwithf "*** TIME 50 ***/n%O" state
    { state with Time = tNext }

let isCompleted state =
    state.Queues.[Factory].IsEmpty && state.Queues.[Port].IsEmpty && List.forall (fun v -> v.Cargo.IsNone) state.Vehicles

let run rules originalState =
    let rec runRec state =
        if isCompleted state then
            state
        else
            let state' = arriveAll state
            match List.tryPick (fun f -> f state') rules with
            | Some state'' -> runRec state''
            | None -> runRec (timePasses state')
    runRec originalState

let scenarioRules =
    [ loadCargo Truck Factory
      loadCargo Ship Port
      despatch Truck Factory (fun cargo -> match cargo with | Destination A -> Port | Destination B -> Warehouse B)
      despatch Ship Port (fun _ -> Warehouse A)
      unload Truck Port
      unload Ship (Warehouse A)
      unload Truck (Warehouse B)
      returnEmpty Truck Port Factory
      returnEmpty Ship (Warehouse A) Port
      returnEmpty Truck (Warehouse B) Factory
    ]

let calculateHours (input: string) =
    let destinations = parseDestinations input
    let cargo = destinations |> List.map Destination

    let state = initialState cargo
    
    let completed = run scenarioRules state

    completed.Time
