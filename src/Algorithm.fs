namespace TransportTycoon

type Destination = A | B

type Cargo =
    | Destination of Destination

type Place =
    | Factory
    | Port
    | Warehouse of Destination

type Location =
    | At of Place
    | Journey of Place * int * Place * int // from, departure time, to, arrival time
    member this.IsWarehouse = match this with | At (Warehouse _) -> true | _ -> false

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

type State =
    {
        Time: int
        FactoryQueue: Cargo list
        PortQueue: Cargo list
        WarehouseA: Cargo list
        WarehouseB: Cargo list
        Vehicles: Vehicle list
    }

module Algorithm =

    let logging = false

    let log msg = if logging then printfn "%s" msg

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
          FactoryQueue = cargo
          PortQueue = []
          WarehouseA = []
          WarehouseB = []
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
        match splitFirstMatch (fun v -> v.Type = Truck && v.Location = At Factory && v.Cargo = None) state.Vehicles, state.FactoryQueue with
        | Some (emptyTruck, otherVehicles), cargoToLoad :: remainingCargo ->
            let loadedVehicle = { emptyTruck with Cargo = Some cargoToLoad }
            sprintf "Loading cargo %O at factory" cargoToLoad |> log
            { state with FactoryQueue = remainingCargo; Vehicles = loadedVehicle :: otherVehicles }
        | _ -> state            

    let loadCargoAtPort state =
        match splitFirstMatch (fun v -> v.Type = Ship && v.Location = At Port && v.Cargo = None) state.Vehicles, state.PortQueue with
        | Some (emptyShip, otherVehicles), cargoToLoad :: remainingCargo ->
            let loadedVehicle = { emptyShip with Cargo = Some cargoToLoad }
            sprintf "Loading cargo %O at port" cargoToLoad |> log
            { state with PortQueue = remainingCargo; Vehicles = loadedVehicle :: otherVehicles }
        | _ -> state  

    let despatchLoadedTruck state =
        match splitFirstMatch (fun v -> v.Type = Truck && v.Location = At Factory && v.Cargo.IsSome) state.Vehicles with
        | Some (loadedTruck, otherVehicles) ->
            let journey =
                match loadedTruck.Cargo.Value with
                | Destination A -> (Factory, state.Time, Port, state.Time + distanceFactoryToPort)
                | Destination B -> (Factory, state.Time, Warehouse B, state.Time + distanceFactoryToB)
            let movingTruck = { loadedTruck with Location = Journey journey }
            sprintf "Despatching loaded truck from factory with cargo %O" loadedTruck.Cargo.Value |> log
            { state with Vehicles = movingTruck :: otherVehicles }
        | _ -> state

    let unloadTruckAtPort state =
        match splitFirstMatch (fun v -> v.Type = Truck && v.Location = At Port && v.Cargo.IsSome) state.Vehicles with
        | Some (loadedTruck, otherVehicles) ->
            let unloadedTruck, cargo = unloadVehicle loadedTruck
            sprintf "Unloading cargo %O at port" cargo |> log
            { state with PortQueue = cargo :: state.PortQueue; Vehicles = unloadedTruck :: otherVehicles }
        | _ -> state        

    let unloadVehicleAtWarehouse state =
        match splitFirstMatch (fun v -> v.Location.IsWarehouse && v.Cargo.IsSome) state.Vehicles with
        | Some (loadedVehicle, otherVehicles) ->
            let unloadedVehicle, cargo = unloadVehicle loadedVehicle
            match loadedVehicle.Location with
            | At (Warehouse A) -> sprintf "Unloading cargo %O at warehouse A" cargo |> log; { state with WarehouseA = cargo :: state.WarehouseA; Vehicles = unloadedVehicle :: otherVehicles }
            | At (Warehouse B) -> sprintf "Unloading cargo %O at warehouse B" cargo |> log; { state with WarehouseB = cargo :: state.WarehouseB; Vehicles = unloadedVehicle :: otherVehicles }
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
            sprintf "Empty truck returning from %O" emptyTruck.Location |> log
            { state with Vehicles = movingTruck :: otherVehicles }
        | _ -> state
    
    let despatchShip state =
        match splitFirstMatch (fun v -> v.Type = Ship && v.Location = At Port && v.Cargo.IsSome) state.Vehicles with
        | Some (loadedShip, otherVehicles) ->
            let journey = (Port, state.Time, Warehouse A, state.Time + distancePortToA)
            let movingShip = { loadedShip with Location = Journey journey }
            sprintf "Ship despatched with cargo %O" loadedShip.Cargo.Value |> log
            { state with Vehicles = movingShip :: otherVehicles }
        | _ -> state

    let returnEmptyShip state =
        match splitFirstMatch (fun v -> v.Type = Ship && not v.IsMoving && v.Location = At (Warehouse A) && v.Cargo.IsNone) state.Vehicles with
        | Some (emptyShip, otherVehicles) ->
            let journey = (Warehouse A, state.Time, Port, state.Time + distancePortToA)
            let movingShip = { emptyShip with Location = Journey journey }
            sprintf "Empty ship returning from %O" emptyShip.Location |> log
            { state with Vehicles = movingShip :: otherVehicles }
        | _ -> state
    
    let arrive state =
        match splitFirstMatch (isArrivingAt state.Time) state.Vehicles with
        | Some (arrivingVehicle, otherVehicles) ->
            let location = match arrivingVehicle.Location with | Journey (_, _, loc, _) -> loc | _ -> failwith "Location not found"
            let arrived = { arrivingVehicle with Location = At location }
            sprintf "Vehicle arrives at %O" location |> log
            { state with Vehicles = arrived :: otherVehicles }
        | _ -> state        

    let timePasses state =
        let tNext = state.Time + 1
        sprintf "- Time %i" tNext |> log
        if tNext > 50 then failwithf "*** TIME 50 ***/n%O" state
        { state with Time = tNext }

    let possibleActions =
        [ loadCargoAtFactory
          loadCargoAtPort
          despatchLoadedTruck
          unloadTruckAtPort
          unloadVehicleAtWarehouse
          returnEmptyTruck
          despatchShip
          returnEmptyShip
          arrive
          timePasses // must be last
        ]

    let updatedStateIfDifferent state action =
        let updated = action state
        if updated = state then None else Some updated

    let tryUpdate state =
        let updatedOpt = List.tryPick (updatedStateIfDifferent state) possibleActions
        updatedOpt

    let isCompleted state =
        state.FactoryQueue.IsEmpty && state.PortQueue.IsEmpty && List.forall (fun v -> v.Cargo.IsNone) state.Vehicles

    let rec go state =
        if isCompleted state then
            state
        else
            match tryUpdate state with
            | Some updated -> go updated
            | None -> failwith "Uh oh"

    let calculateHours (input: string) =
        let destinations = parseDestinations input
        let cargo = destinations |> List.map Destination

        let state = initialState cargo
        
        let completed = go state

        completed.Time
        
