module TransportTycoon.Exercise1

open Algorithm

let distances location1 location2 =
    match location1, location2 with
    | Factory, Port        | Port, Factory        -> 1
    | Port, Warehouse A    | Warehouse A, Port    -> 4
    | Factory, Warehouse B | Warehouse B, Factory -> 5
    | _ -> failwith "Impossible journey"

let initialVehicles =
    [ { Id = 0; Type = Truck; Capacity = 1; Location = At Factory; Cargo = []; Loading = [] }
      { Id = 1; Type = Truck; Capacity = 1; Location = At Factory; Cargo = []; Loading = [] }
      { Id = 2; Type = Ship; Capacity = 1; Location = At Port; Cargo = []; Loading = [] }
    ]

let initialState log cargo =
    { Time = 0
      Queues = [ (Factory, cargo); (Port, []); (Warehouse A, []); (Warehouse B, []) ] |> Map.ofList
      Vehicles = initialVehicles
      Distances = distances
      LoadTimes = fun _ -> 0
      Log = log }

let scenarioRules =
    [ unload Truck Port
      unload Ship (Warehouse A)
      unload Truck (Warehouse B)
      loadCargo Truck Factory
      loadCargo Ship Port
      despatch Truck Factory alwaysDespatch (fun cargo -> match cargo.Destination with | A -> Port | B -> Warehouse B)
      despatch Ship Port alwaysDespatch (fun _ -> Warehouse A)
      returnEmpty Truck Port Factory
      returnEmpty Ship (Warehouse A) Port
      returnEmpty Truck (Warehouse B) Factory
    ]

let calculateHours logger destinations =
    let cargo = destinations |> List.mapi (fun i dest -> { Id = i; Destination = dest; Origin = Factory } )
    let state = initialState logger cargo
    let completed = run scenarioRules state

    completed.Time
