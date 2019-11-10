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

    let loadCargo vehicleType location state =
        match splitFirstMatch (fun v -> v.Type = vehicleType && v.Location = At location && v.Cargo = None) state.Vehicles, state.Queues.[location] with
        | Some (emptyVehicle, otherVehicles), cargoToLoad :: remainingCargo ->
            let loadedVehicle = { emptyVehicle with Cargo = Some cargoToLoad }
            sprintf "Loading: %O onto %O" cargoToLoad emptyVehicle |> log state
            { state with Queues = state.Queues.Add(location, remainingCargo); Vehicles = loadedVehicle :: otherVehicles }
        | _ -> state            

    let despatch vehicleType location state =
        match splitFirstMatch (fun v -> v.Type = vehicleType && v.Location = At location && v.Cargo.IsSome) state.Vehicles with
        | Some (loadedVehicle, otherVehicles) ->
            let destination =
                match location, loadedVehicle.Cargo.Value with
                | Factory, Destination A -> Port
                | Factory, Destination B -> Warehouse B
                | Port, Destination A -> Warehouse A
                | _ -> failwith "Unknown despatch"
            let journey = (location, state.Time, destination, state.Time + distance location destination)
            let movingVehicle = { loadedVehicle with Location = Journey journey }
            sprintf "Despatching: %O, %O" loadedVehicle movingVehicle.Location |> log state
            { state with Vehicles = movingVehicle :: otherVehicles }
        | _ -> state
    
    let unload vehicleType location state =
        match splitFirstMatch (fun v -> v.Type = vehicleType && v.Location = At location && v.Cargo.IsSome) state.Vehicles with
        | Some (loadedVehicle, otherVehicles) ->
            let unloadedVehicle, cargo = unloadVehicle loadedVehicle
            sprintf "Unloading: %O" loadedVehicle |> log state
            { state with Queues = state.Queues.Add(location, cargo :: state.Queues.[location]); Vehicles = unloadedVehicle :: otherVehicles }
        | _ -> state        

    let returnEmptyVehicle vehicleType location state =
        match splitFirstMatch (fun v -> v.Type = vehicleType && v.Location = At location && v.Cargo.IsNone) state.Vehicles with
        | Some (emptyVehicle, otherVehicles) ->
            let destination =
                match location with
                | Port -> Factory
                | Warehouse A -> Port
                | Warehouse B -> Factory
                | _ -> failwith "Unknown return"
            let journey = (location, state.Time, destination, state.Time + distance location destination)
            let movingVehicle = { emptyVehicle with Location = Journey journey }
            sprintf "Returning: %O, %O" emptyVehicle movingVehicle.Location |> log state
            { state with Vehicles = movingVehicle :: otherVehicles }
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
          loadCargo Truck Factory
          loadCargo Ship Port
          despatch Truck Factory
          despatch Ship Port
          unload Truck Port
          unload Ship (Warehouse A)
          unload Truck (Warehouse B)
          returnEmptyVehicle Truck Port
          returnEmptyVehicle Ship (Warehouse A)
          returnEmptyVehicle Truck (Warehouse B)
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
        
