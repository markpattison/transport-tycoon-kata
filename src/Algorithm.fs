module TransportTycoon.Algorithm

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
    | Some (loadedVehicle, otherVehicles) -> Some (loadedVehicle, loadedVehicle.Cargo.Value, otherVehicles)
    | _ -> None

let (|CargoAt|_|) location state =
    match state.Queues.[location] with
    | cargoToLoad :: remainingCargo -> Some (cargoToLoad, remainingCargo)
    | [] -> None

let loadCargo vehicleType location state =
    match state with
    | EmptyVehicleAt vehicleType location (emptyVehicle, otherVehicles) & CargoAt location (cargoToLoad, remainingCargo) ->
        let loadedVehicle = { emptyVehicle with Cargo = Some cargoToLoad }
        Some { state with Queues = state.Queues.Add(location, remainingCargo); Vehicles = loadedVehicle :: otherVehicles }
    | _ -> None    

let despatch vehicleType location findDestination state =
    match state with
    | LoadedVehicleAt vehicleType location (loadedVehicle, _, otherVehicles) ->
        let destination = findDestination loadedVehicle.Cargo.Value
        let journey = (location, state.Time, destination, state.Time + state.Distances location destination)
        let movingVehicle = { loadedVehicle with Location = Journey journey }
        sprintf "Despatching: %O, %O" loadedVehicle movingVehicle.Location |> state.Log
        Some { state with Vehicles = movingVehicle :: otherVehicles }
    | _ -> None    

let unload vehicleType location state =
    match state with
    | LoadedVehicleAt vehicleType location (loadedVehicle, cargo, otherVehicles) ->
        let unloadedVehicle = { loadedVehicle with Cargo = None }
        Some { state with Queues = state.Queues.Add(location, cargo :: state.Queues.[location]); Vehicles = unloadedVehicle :: otherVehicles }
    | _ -> None    

let returnEmpty vehicleType location destination state =
    match state with
    | EmptyVehicleAt vehicleType location (emptyVehicle, otherVehicles) ->
        let journey = (location, state.Time, destination, state.Time + state.Distances location destination)
        let movingVehicle = { emptyVehicle with Location = Journey journey }
        sprintf "Returning: %O, %O" emptyVehicle movingVehicle.Location |> state.Log
        Some { state with Vehicles = movingVehicle :: otherVehicles }
    | _ -> None    

let arriveAll state =
    let arrive vehicle =
        match vehicle.Location with
        | Journey (_, _, location, t) when t = state.Time ->
            { vehicle with Location = At location }
        | _ -> vehicle
    
    { state with Vehicles = state.Vehicles |> List.map arrive }

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
