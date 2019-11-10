namespace TransportTycoon

type Destination = A | B

type Cargo =
    | Destination of Destination
    override this.ToString() =
        match this with
        | Destination d -> sprintf "cargo for %O" d

type Place =
    | Factory
    | Port
    | Warehouse of Destination

type Location =
    | At of Place
    | Journey of Place * int * Place * int // from, departure time, to, arrival time
    member this.IsWarehouse = match this with | At (Warehouse _) -> true | _ -> false
    override this.ToString() =
        match this with
        | At place -> sprintf "at %O" place
        | Journey (from, leftAt, goingTo, arrivingAt) -> sprintf "travelling from %O (left at %i) to %O (arriving at %i)" from leftAt goingTo arrivingAt

type VehicleType =
    | Truck
    | Ship

type Vehicle =
    {
        Type: VehicleType
        Location: Location
        Cargo: Cargo option
    }
    member this.IsMoving = match this.Location with | Journey _ -> true | _ -> false
    override this.ToString() =
        match this.Cargo with
        | None -> sprintf "empty %O %O" this.Type this.Location
        | Some cargo -> sprintf "%O (%O) %O" this.Type cargo this.Location

type State =
    {
        Time: int
        Queues: Map<Place, Cargo list>
        Vehicles: Vehicle list
    }

module Algorithm =

    let logging = false

    let log state msg = if logging then printfn "Time %i: %s" state.Time msg

    let distanceFactoryToPort = 1
    let distancePortToA = 4
    let distanceFactoryToB = 5

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

    let isArrivingAt time v =
        match v.Location with
        | Journey (_, _, loc, t) when t = time -> true
        | _ -> false

    let splitFirstMatch predicate lst =
        let rec split acc l =
            match l with
            | x :: xs when predicate x -> Some (x, acc @ xs)
            | x :: xs -> split (acc @ [ x ]) xs
            | [] -> None
        split [] lst

    let loadCargoAtFactory state =
        match splitFirstMatch (fun v -> v.Type = Truck && v.Location = At Factory && v.Cargo = None) state.Vehicles, state.Queues.[Factory] with
        | Some (emptyTruck, otherVehicles), cargoToLoad :: remainingCargo ->
            let loadedVehicle = { emptyTruck with Cargo = Some cargoToLoad }
            sprintf "Loading: %O onto %O" cargoToLoad emptyTruck |> log state
            { state with Queues = state.Queues.Add(Factory, remainingCargo); Vehicles = loadedVehicle :: otherVehicles }
        | _ -> state            

    let loadCargoAtPort state =
        match splitFirstMatch (fun v -> v.Type = Ship && v.Location = At Port && v.Cargo = None) state.Vehicles, state.Queues.[Port] with
        | Some (emptyShip, otherVehicles), cargoToLoad :: remainingCargo ->
            let loadedVehicle = { emptyShip with Cargo = Some cargoToLoad }
            sprintf "Loading: %O onto %O" cargoToLoad emptyShip |> log state
            { state with Queues = state.Queues.Add(Port, remainingCargo); Vehicles = loadedVehicle :: otherVehicles }
        | _ -> state  

    let despatchLoadedTruck state =
        match splitFirstMatch (fun v -> v.Type = Truck && v.Location = At Factory && v.Cargo.IsSome) state.Vehicles with
        | Some (loadedTruck, otherVehicles) ->
            let journey =
                match loadedTruck.Cargo.Value with
                | Destination A -> (Factory, state.Time, Port, state.Time + distanceFactoryToPort)
                | Destination B -> (Factory, state.Time, Warehouse B, state.Time + distanceFactoryToB)
            let movingTruck = { loadedTruck with Location = Journey journey }
            sprintf "Despatching: %O, %O" loadedTruck movingTruck.Location |> log state
            { state with Vehicles = movingTruck :: otherVehicles }
        | _ -> state

    let unloadTruckAtPort state =
        match splitFirstMatch (fun v -> v.Type = Truck && v.Location = At Port && v.Cargo.IsSome) state.Vehicles with
        | Some (loadedTruck, otherVehicles) ->
            let unloadedTruck, cargo = unloadVehicle loadedTruck
            sprintf "Unloading: %O" loadedTruck |> log state
            { state with Queues = state.Queues.Add(Port, cargo :: state.Queues.[Port]); Vehicles = unloadedTruck :: otherVehicles }
        | _ -> state        

    let unloadVehicleAtWarehouse state =
        match splitFirstMatch (fun v -> v.Location.IsWarehouse && v.Cargo.IsSome) state.Vehicles with
        | Some (loadedVehicle, otherVehicles) ->
            let unloadedVehicle, cargo = unloadVehicle loadedVehicle
            match loadedVehicle.Location with
            | At (Warehouse A) -> sprintf "Unloading: %O" loadedVehicle |> log state; { state with Queues = state.Queues.Add(Warehouse A, cargo :: state.Queues.[Warehouse A]); Vehicles = unloadedVehicle :: otherVehicles }
            | At (Warehouse B) -> sprintf "Unloading: %O" loadedVehicle |> log state; { state with Queues = state.Queues.Add(Warehouse B, cargo :: state.Queues.[Warehouse B]); Vehicles = unloadedVehicle :: otherVehicles }
            | _ -> failwith "Impossible to unload"
        | _ -> state 
    
    let returnEmptyTruck state =
        match splitFirstMatch (fun v -> v.Type = Truck && not v.IsMoving && v.Location <> At Factory && v.Cargo.IsNone) state.Vehicles with
        | Some (emptyTruck, otherVehicles) ->
            let journey =
                match emptyTruck.Location with
                | At Port -> (Port, state.Time, Factory, state.Time + distanceFactoryToPort)
                | At (Warehouse B) -> (Warehouse B, state.Time, Factory, state.Time + distanceFactoryToB)
                | _ -> failwith "Impossible truck"
            let movingTruck = { emptyTruck with Location = Journey journey }
            sprintf "Returning: %O, %O" emptyTruck movingTruck.Location |> log state
            { state with Vehicles = movingTruck :: otherVehicles }
        | _ -> state
    
    let despatchShip state =
        match splitFirstMatch (fun v -> v.Type = Ship && v.Location = At Port && v.Cargo.IsSome) state.Vehicles with
        | Some (loadedShip, otherVehicles) ->
            let journey = (Port, state.Time, Warehouse A, state.Time + distancePortToA)
            let movingShip = { loadedShip with Location = Journey journey }
            sprintf "Despatching: %O, %O" loadedShip movingShip.Location |> log state
            { state with Vehicles = movingShip :: otherVehicles }
        | _ -> state

    let returnEmptyShip state =
        match splitFirstMatch (fun v -> v.Type = Ship && not v.IsMoving && v.Location = At (Warehouse A) && v.Cargo.IsNone) state.Vehicles with
        | Some (emptyShip, otherVehicles) ->
            let journey = (Warehouse A, state.Time, Port, state.Time + distancePortToA)
            let movingShip = { emptyShip with Location = Journey journey }
            sprintf "Returning: %O, %O" emptyShip movingShip.Location |> log state
            { state with Vehicles = movingShip :: otherVehicles }
        | _ -> state
    
    let arrive state =
        match splitFirstMatch (isArrivingAt state.Time) state.Vehicles with
        | Some (arrivingVehicle, otherVehicles) ->
            let location = match arrivingVehicle.Location with | Journey (_, _, loc, _) -> loc | _ -> failwith "Location not found"
            let arrived = { arrivingVehicle with Location = At location }
            sprintf "Arriving: %O" arrivingVehicle |> log state
            { state with Vehicles = arrived :: otherVehicles }
        | _ -> state        

    let timePasses state =
        let tNext = state.Time + 1
        if tNext > 50 then failwithf "*** TIME 50 ***/n%O" state
        { state with Time = tNext }

    let possibleActions =
        [ arrive
          loadCargoAtFactory
          loadCargoAtPort
          despatchLoadedTruck
          despatchShip
          unloadTruckAtPort
          unloadVehicleAtWarehouse
          returnEmptyTruck
          returnEmptyShip
          timePasses // must be last
        ]

    let updatedStateIfDifferent state action =
        let updated = action state
        if updated = state then None else Some updated

    let tryUpdate state =
        let updatedOpt = List.tryPick (updatedStateIfDifferent state) possibleActions
        updatedOpt

    let isCompleted state =
        state.Queues.[Factory].IsEmpty && state.Queues.[Port].IsEmpty && List.forall (fun v -> v.Cargo.IsNone) state.Vehicles

    let rec run state =
        if isCompleted state then
            state
        else
            match tryUpdate state with
            | Some updated -> run updated
            | None -> failwith "Uh oh"

    let calculateHours (input: string) =
        let destinations = parseDestinations input
        let cargo = destinations |> List.map Destination

        let state = initialState cargo
        
        let completed = run state

        completed.Time
        
