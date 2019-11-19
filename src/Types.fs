namespace TransportTycoon

type Destination = A | B

type Place =
    | Factory
    | Port
    | Warehouse of Destination
    override this.ToString() =
        match this with
        | Factory -> "FACTORY"
        | Port -> "PORT"
        | Warehouse dest -> dest.ToString()

type Cargo =
    {
        Id: int
        Destination: Destination
        Origin: Place
    }

type Location =
    | At of Place
    | Journey of Place * int * Place * int // from, departure time, to, arrival time

type VehicleType =
    | Truck
    | Ship
    override this.ToString() =
        match this with
        | Truck -> "TRUCK"
        | Ship -> "SHIP"

type Vehicle =
    {
        Id: int
        Type: VehicleType
        Capacity: int
        Location: Location
        Cargo: Cargo list
        Loading: (Cargo * int) list // finish time
    }
    member this.TotalCargo = this.Cargo.Length + this.Loading.Length

type State =
    {
        Time: int
        Queues: Map<Place, Cargo list>
        Vehicles: Vehicle list
        LoadTimes: VehicleType -> int
        Distances: Place -> Place -> int
        Log: string -> unit
    }
