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
    override this.ToString() =
        match this with
        | Factory -> "FACTORY"
        | Port -> "PORT"
        | Warehouse dest -> dest.ToString()

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
    override this.ToString() =
        match this with
        | Truck -> "TRUCK"
        | Ship -> "SHIP"

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
        Distances: Place -> Place -> int
    }
