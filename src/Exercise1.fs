module TransportTycoon.Exercise1

open Algorithm

let distances location1 location2 =
    match location1, location2 with
    | Factory, Port        | Port, Factory        -> 1
    | Port, Warehouse A    | Warehouse A, Port    -> 4
    | Factory, Warehouse B | Warehouse B, Factory -> 5
    | _ -> failwith "Impossible journey"

let initialVehicles =
    [ { Id = 0; Type = Truck; Location = At Factory; Cargo = None }
      { Id = 1; Type = Truck; Location = At Factory; Cargo = None }
      { Id = 2; Type = Ship; Location = At Port; Cargo = None }
    ]

let initialState cargo =
    { Time = 0
      Queues = [ (Factory, cargo); (Port, []); (Warehouse A, []); (Warehouse B, []) ] |> Map.ofList
      Vehicles = initialVehicles
      Distances = distances }

let scenarioRules =
    [ loadCargo Truck Factory
      loadCargo Ship Port
      despatch Truck Factory (fun cargo -> match cargo.Destination with | A -> Port | B -> Warehouse B)
      despatch Ship Port (fun _ -> Warehouse A)
      unload Truck Port
      unload Ship (Warehouse A)
      unload Truck (Warehouse B)
      returnEmpty Truck Port Factory
      returnEmpty Ship (Warehouse A) Port
      returnEmpty Truck (Warehouse B) Factory
    ]

let calculateHours destinations =
    let cargo = destinations |> List.mapi (fun i dest -> { Id = i; Destination = dest } )
    let state = initialState cargo
    let completed = run scenarioRules state

    completed.Time
