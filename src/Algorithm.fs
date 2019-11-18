module TransportTycoon.Algorithm

let splitFirstMatch predicate lst =
    let rec split acc l =
        match l with
        | x :: xs when predicate x -> Some (x, acc @ xs)
        | x :: xs -> split (acc @ [ x ]) xs
        | [] -> None
    split [] lst

let (|EmptyVehicleAt|_|) vehicleType location state =
    match splitFirstMatch (fun v -> v.Type = vehicleType && v.Location = At location && v.Cargo.IsEmpty) state.Vehicles with
    | Some (emptyVehicle, otherVehicles) -> Some (emptyVehicle, otherVehicles)
    | _ -> None

let (|VehicleNotFullAt|_|) vehicleType location state =
    match splitFirstMatch (fun v -> v.Type = vehicleType && v.Location = At location && v.Cargo.Length < v.Capacity) state.Vehicles with
    | Some (notFullVehicle, otherVehicles) -> Some (notFullVehicle, otherVehicles)
    | _ -> None

let (|LoadedVehicleAt|_|) vehicleType location state =
    match splitFirstMatch (fun v -> v.Type = vehicleType && v.Location = At location && not v.Cargo.IsEmpty) state.Vehicles with
    | Some (loadedVehicle, otherVehicles) -> Some (loadedVehicle, otherVehicles)
    | _ -> None

let (|CargoAt|_|) location state =
    match state.Queues.[location] with
    | cargoToLoad :: remainingCargo -> Some (cargoToLoad, remainingCargo)
    | [] -> None

let singleCargoLogString (cargo: Cargo) =
    sprintf """{"cargo_id": %i, "destination": %O, "origin": "%O"}""" cargo.Id cargo.Destination cargo.Origin

let cargoLogString cargo =
    match cargo with
    | [] -> ""
    | _ ->
        let inner = System.String.Join(", ", cargo |> List.map singleCargoLogString)
        sprintf """, "cargo": [%s]""" inner

let logDepart state vehicle location destination =
    sprintf """{"event": "DEPART", "time": %i, "transport_id": %i, "kind": "%O", "location": "%O", "destination": %O%s}""" state.Time vehicle.Id vehicle.Type location destination (cargoLogString vehicle.Cargo)
    |> state.Log

let logArrive state vehicle location =
    sprintf """{"event": "ARRIVE", "time": %i, "transport_id": %i, "kind": "%O", "location": "%O"%s}""" state.Time vehicle.Id vehicle.Type location (cargoLogString vehicle.Cargo)
    |> state.Log

let loadCargo vehicleType location state =
    match state with
    | VehicleNotFullAt vehicleType location (notFullVehicle, otherVehicles) & CargoAt location (cargoToLoad, remainingCargo) ->
        let loadedVehicle = { notFullVehicle with Cargo = cargoToLoad :: notFullVehicle.Cargo }
        Some { state with Queues = state.Queues.Add(location, remainingCargo); Vehicles = loadedVehicle :: otherVehicles }
    | _ -> None

let despatch vehicleType location findDestination state =
    match state with
    | LoadedVehicleAt vehicleType location (loadedVehicle, otherVehicles) ->
        let destination = findDestination loadedVehicle.Cargo.Head
        let journey = (location, state.Time, destination, state.Time + state.Distances location destination)
        let movingVehicle = { loadedVehicle with Location = Journey journey }

        logDepart state movingVehicle location destination

        Some { state with Vehicles = movingVehicle :: otherVehicles }
    | _ -> None

let unload vehicleType location state =
    match state with
    | LoadedVehicleAt vehicleType location (loadedVehicle, otherVehicles) ->
        let unloadedVehicle = { loadedVehicle with Cargo = loadedVehicle.Cargo.Tail }
        Some { state with Queues = state.Queues.Add(location, loadedVehicle.Cargo.Head :: state.Queues.[location]); Vehicles = unloadedVehicle :: otherVehicles }
    | _ -> None

let returnEmpty vehicleType location destination state =
    match state with
    | EmptyVehicleAt vehicleType location (emptyVehicle, otherVehicles) ->
        let journey = (location, state.Time, destination, state.Time + state.Distances location destination)
        let movingVehicle = { emptyVehicle with Location = Journey journey }

        logDepart state movingVehicle location destination

        Some { state with Vehicles = movingVehicle :: otherVehicles }
    | _ -> None

let arriveAll state =
    let arrive vehicle =
        match vehicle.Location with
        | Journey (_, _, location, t) when t = state.Time ->
            logArrive state vehicle location
            { vehicle with Location = At location }
        | _ -> vehicle
    
    { state with Vehicles = state.Vehicles |> List.map arrive }

let timePasses state =
    let tNext = state.Time + 1
    if tNext > 50 then failwithf "*** TIME 50 ***/n%O" state
    { state with Time = tNext }

let isCompleted state =
    state.Queues.[Factory].IsEmpty && state.Queues.[Port].IsEmpty && List.forall (fun v -> v.Cargo.IsEmpty) state.Vehicles

let run rules originalState =
    let rec runRec state =
        let state' = arriveAll state
        match List.tryPick (fun f -> f state') rules with
        | Some state'' -> runRec state''
        | None ->
            if isCompleted state' then
                state'
            else
                runRec (timePasses state')
    runRec originalState
