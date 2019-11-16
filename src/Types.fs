namespace TransportTycoon

type Destination = A | B

type Cargo =
    {
        Id: int
        Destination: Destination
    }

type Place =
    | Factory
    | Port
    | Warehouse of Destination
    override this.ToString() =
        match this with
        | Factory -> "FACTORY"
        | Port -> "PORT"
        | Warehouse dest -> dest.ToString()

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
        Location: Location
        Cargo: Cargo option
    }

type State =
    {
        Time: int
        Queues: Map<Place, Cargo list>
        Vehicles: Vehicle list
        Distances: Place -> Place -> int
    }
