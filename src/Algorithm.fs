module TransportTycoon.Algorithm

let alwaysDespatch = fun _ -> true

let splitFirstMatch predicate lst =
    let rec split acc l =
        match l with
        | x :: xs when predicate x -> Some (x, acc @ xs)
        | x :: xs -> split (acc @ [ x ]) xs
        | [] -> None
    split [] lst

// active patterns

let (|EmptyVehicleAt|_|) vehicleType location state =
    match splitFirstMatch (fun v -> v.Type = vehicleType && v.Location = At location && v.TotalCargo = 0) state.Vehicles with
    | Some (emptyVehicle, otherVehicles) -> Some (emptyVehicle, otherVehicles)
    | _ -> None

let (|VehicleNotFullAt|_|) vehicleType location state =
    match splitFirstMatch (fun v -> v.Type = vehicleType && v.Location = At location && v.TotalCargo < v.Capacity) state.Vehicles with
    | Some (notFullVehicle, otherVehicles) -> Some (notFullVehicle, otherVehicles)
    | _ -> None

let (|VehicleCanDespatchAt|_|) vehicleType location state =
    match splitFirstMatch (fun v -> v.Type = vehicleType && v.Location = At location && v.Cargo.Length > 0 && v.Loading.Length = 0 && v.Unloading.Length = 0) state.Vehicles with
    | Some (loadedVehicle, otherVehicles) -> Some (loadedVehicle, otherVehicles)
    | _ -> None

let (|VehicleCanUnloadAt|_|) vehicleType location state =
    match splitFirstMatch (fun v -> v.Type = vehicleType && v.Location = At location && v.Cargo.Length > 0) state.Vehicles with
    | Some (loadedVehicle, otherVehicles) -> Some (loadedVehicle, otherVehicles)
    | _ -> None

let (|CargoAt|_|) location state =
    match state.Queues.[location] with
    | cargoToLoad :: remainingCargo -> Some (cargoToLoad, remainingCargo)
    | [] -> None

// logging

let singleCargoLogString (cargo: Cargo) =
    sprintf """{"cargo_id": %i, "destination": "%O", "origin": "%O"}""" cargo.Id cargo.Destination cargo.Origin

let cargoLogString cargo =
    match cargo with
    | [] -> ""
    | _ ->
        let inner = System.String.Join(", ", cargo |> List.map singleCargoLogString)
        sprintf """, "cargo": [%s]""" inner

let logDepart state vehicle location destination =
    sprintf """{"event": "DEPART", "time": %i, "transport_id": %i, "kind": "%O", "location": "%O", "destination": "%O"%s}""" state.Time vehicle.Id vehicle.Type location destination (cargoLogString vehicle.Cargo)
    |> state.Log

let logArrive state vehicle location =
    sprintf """{"event": "ARRIVE", "time": %i, "transport_id": %i, "kind": "%O", "location": "%O"%s}""" state.Time vehicle.Id vehicle.Type location (cargoLogString vehicle.Cargo)
    |> state.Log

let logLoad state vehicle location cargo =
    sprintf """{"event": "LOAD", "time": %i, "transport_id": %i, "kind": "%O", "location": "%O", "cargo": %s}""" state.Time vehicle.Id vehicle.Type location (singleCargoLogString cargo)
    |> state.Log

let logUnload state vehicle location cargo =
    sprintf """{"event": "UNLOAD", "time": %i, "transport_id": %i, "kind": "%O", "location": "%O", "cargo": %s}""" state.Time vehicle.Id vehicle.Type location (singleCargoLogString cargo)
    |> state.Log

// updates

let loadCargo vehicleType location state =
    match state with
    | VehicleNotFullAt vehicleType location (notFullVehicle, otherVehicles) & CargoAt location (cargoToLoad, remainingCargo) ->
        let loadingVehicle = { notFullVehicle with Loading = (cargoToLoad, state.Time + state.LoadTimes vehicleType) :: notFullVehicle.Loading }

        logLoad state loadingVehicle location cargoToLoad

        Some { state with Queues = state.Queues.Add(location, remainingCargo); Vehicles = loadingVehicle :: otherVehicles }
    | _ -> None

let despatch vehicleType location shouldDespatch findDestination state =
    match state with
    | VehicleCanDespatchAt vehicleType location (vehicle, otherVehicles) when vehicle.Cargo.Length = vehicle.Capacity || shouldDespatch state ->
        let destination = findDestination vehicle.Cargo.Head
        let journey = (location, state.Time, destination, state.Time + state.Distances location destination)
        let movingVehicle = { vehicle with Location = Journey journey }

        logDepart state movingVehicle location destination

        Some { state with Vehicles = movingVehicle :: otherVehicles }
    | _ -> None

let unload vehicleType location state =
    match state with
    | VehicleCanUnloadAt vehicleType location (loadedVehicle, otherVehicles) ->
        let unloadingVehicle = { loadedVehicle with Cargo = loadedVehicle.Cargo.Tail; Unloading = (loadedVehicle.Cargo.Head, state.Time + state.UnloadTimes vehicleType) :: loadedVehicle.Unloading }
        Some { state with Vehicles = unloadingVehicle :: otherVehicles }
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

let finishLoadingAll state =
    let finishLoading vehicle =
        let cargoLoaded, otherCargo = List.partition (fun (_, t) -> t = state.Time) vehicle.Loading
        { vehicle with Cargo = List.map fst cargoLoaded @ vehicle.Cargo; Loading = otherCargo }
    
    { state with Vehicles = state.Vehicles |> List.map finishLoading }

let finishUnloadingAll state =
    let finishUnloading vehicle =
        match vehicle.Location with
        | At location ->
            let cargoUnloaded, otherCargo = List.partition (fun (_, t) -> t = state.Time) vehicle.Unloading

            cargoUnloaded |> List.iter (fun (c, _) -> logUnload state vehicle location c)

            { vehicle with Unloading = otherCargo }, (cargoUnloaded |> List.map (fun (c, _) -> location, c))
        | _ -> vehicle, []
    
    let updatedVehicles, unloadedCargo = state.Vehicles |> List.map finishUnloading |> List.unzip
    let unloadedCargoByPlace = unloadedCargo |> List.concat |> List.groupBy fst |> List.map (fun (loc, cargo) -> loc, cargo |> List.map snd)

    let updatedQueues = unloadedCargoByPlace |> List.fold (fun (queues: Map<Place, list<Cargo>>) (location, unloaded) -> queues.Add(location, unloaded @ state.Queues.[location])) state.Queues

    { state with Queues = updatedQueues; Vehicles = updatedVehicles }

let timePasses state =
    let tNext = state.Time + 1
    if tNext > 50 then failwithf "*** TIME 50 ***/n%O" state
    { state with Time = tNext }

let isCompleted state =
    state.Queues.[Factory].IsEmpty && state.Queues.[Port].IsEmpty && state.Vehicles |> List.forall (fun v -> v.TotalCargo = 0) 

let run rules originalState =
    let rec runRec state =
        let state' = state |> finishLoadingAll |> arriveAll |> finishUnloadingAll
        match List.tryPick (fun f -> f state') rules with
        | Some state'' ->
            runRec state''
        | None ->
            if isCompleted state' then
                state'
            else
                runRec (timePasses state')
    runRec originalState
