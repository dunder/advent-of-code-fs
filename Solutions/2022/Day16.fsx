// --- Day 16: Proboscidea Volcanium ---

open System.Collections.Generic
open System.IO
open System.Text.RegularExpressions

System.Environment.CurrentDirectory <- __SOURCE_DIRECTORY__


let input = File.ReadLines(@".\Input\Day16.txt") |> List.ofSeq

let example = 
    [
        "Valve AA has flow rate=0; tunnels lead to valves DD, II, BB"
        "Valve BB has flow rate=13; tunnels lead to valves CC, AA"
        "Valve CC has flow rate=2; tunnels lead to valves DD, BB"
        "Valve DD has flow rate=20; tunnels lead to valves CC, AA, EE"
        "Valve EE has flow rate=3; tunnels lead to valves FF, DD"
        "Valve FF has flow rate=0; tunnels lead to valves EE, GG"
        "Valve GG has flow rate=0; tunnels lead to valves FF, HH"
        "Valve HH has flow rate=22; tunnel leads to valve GG"
        "Valve II has flow rate=0; tunnels lead to valves AA, JJ"
        "Valve JJ has flow rate=21; tunnel leads to valve II"
    ]

let TTL = 30    

type Room = { Name: string; FlowRate: int; TunnelTo: string list }
type RoomIdentity = { Name: string; Time: int; TotalFlow: int }

type RoomState = 
    { Name: string; ValveState: Map<string, bool>; Time: int; TotalFlow: int; Parent: RoomState option }
    member this.Open = this.ValveState[this.Name]
    member this.Identity = { Name = this.Name; Time = this.Time; TotalFlow = this.TotalFlow }
    member this.AllOpen (rooms: Map<string, Room>) =
        this.ValveState 
        |> Map.toSeq 
        |> Seq.forall (fun (valve, isOpen) -> 
            isOpen || rooms[valve].FlowRate = 0)

let parse (lines: string list) = 
    lines
    |> List.map (fun line ->
        let parts = line.Split("; ")
        let m = Regex.Match(parts[0], "Valve (?<valve>[A-Z]{2}) has flow rate=(?<flowRate>\d+)")
        let valveName = m.Groups["valve"].Value
        let flowRate = m.Groups["flowRate"].Value |> int
        // "tunnels lead to valves? (?<nextValves>.*)" does not hit single valves for some reason
        let nextValves =
            if parts[1].Contains("valves") then
                let m2 = Regex.Match(parts[1], "tunnels lead to valves? (?<nextValves>[A-Z ,]*)", RegexOptions.None)
                m2.Groups["nextValves"].Value.Split(", ") |> Array.toList
            else
                [parts[1].Substring("tunnel leads to valve ".Length)]
    
        valveName, { Name = valveName; FlowRate = flowRate; TunnelTo = nextValves }
    )
    |> Map.ofList

let initialValveState (rooms: Map<string, Room>) =
    rooms
    |> Map.values
    |> Seq.map (fun room -> room.Name, false)
    |> Map.ofSeq

let neighbours (rooms: Map<string, Room>) (room: RoomState) =
    if room.AllOpen rooms then
        [{ room with Time = room.Time + 1 }]
    else
        let adjacentRooms = 
            rooms[room.Name].TunnelTo
            |> List.map (fun name -> { 
                Name = name
                ValveState = room.ValveState
                Time = room.Time + 1
                TotalFlow = room.TotalFlow
                Parent = Some(room) })

        if not room.Open && rooms[room.Name].FlowRate > 0 then
            let nextTime = room.Time + 1
            { room with 
                ValveState = room.ValveState |> Map.add room.Name true
                Time = nextTime
                TotalFlow = room.TotalFlow + rooms[room.Name].FlowRate*(TTL-nextTime)
            }::adjacentRooms
        else
            adjacentRooms


let exampleRooms = example |> parse
let exampleValveState = exampleRooms |> initialValveState
let exampleStartRoom = { Name = "AA"; ValveState = exampleValveState; Time = 0; TotalFlow = 0; Parent = None }

type nodeIdentity<'node,'identity> = 'node -> 'identity
let roomIdentity (room: RoomState) = room.Identity

let nonVisitedNeighbours (toNodeIdentity: nodeIdentity<RoomState, RoomIdentity>) rooms (visited:HashSet<RoomIdentity>) room =
    neighbours rooms room
    |> List.filter (fun neighbour -> visited.Add(neighbour |> toNodeIdentity))

let isSolution state = state.Time = TTL

let bfs isSolution nonVisitedNeighbours startState =
    let visited = new HashSet<RoomIdentity>()
    let queue = new Queue<RoomState*int>()
    let rec bfsSearch() =
        if queue.Count = 0 then
            failwith "no solution found"
        else
            let (head, depth) = queue.Dequeue()
            if isSolution head then 
                queue
            else 
                for neighbour in nonVisitedNeighbours visited head do
                    queue.Enqueue (neighbour, depth+1)
                bfsSearch()
    
    queue.Enqueue (startState,0)
    
    bfsSearch()

let openedUp roomState =
    roomState.ValveState
    |> Map.toSeq
    |> Seq.filter(fun (valve, isOpen) -> 
        match roomState.Parent with
        | Some state -> isOpen && (not state.ValveState[valve])
        | None -> isOpen)
    |> Seq.toList

let rec print rooms (roomState: RoomState) =
    let time = TTL - roomState.Time + 1
    let opened = roomState |> openedUp
    printfn "%2i: %s Total Flow: %i Opened: %A All opened: %b" 
        time
        roomState.Name 
        roomState.TotalFlow
        opened
        (roomState.AllOpen rooms)
    match roomState.Parent with
    | Some state -> print rooms state
    | None -> printfn "Back to where we started"

let firstStar =
    
    let rooms = input |> parse
    let valveState = rooms |> initialValveState
    let startRoom = { Name = "AA"; ValveState = valveState; Time = 0; TotalFlow = 0; Parent = None }


    let queue = bfs isSolution (nonVisitedNeighbours roomIdentity rooms) startRoom
    
    queue 
    |> Seq.toList
    |> Seq.map fst
    |> Seq.filter (fun state -> state.Time = TTL)    
    |> Seq.sortBy (fun state -> state.TotalFlow)
    |> Seq.last
    |> print rooms
    //|> (fun state -> state.TotalFlow)

firstStar

let TTL2 = 26

type RoomIdentity2 = { MyRoomName: string; ElephantsRoomName: string; Time: int; TotalFlow: int }
type RoomState2 = 
    { MyRoom: string; ElephantsRoom: string; ValveState: Map<string, bool>; Time: int; TotalFlow: int; Parent: RoomState2 option }
    member this.IsOpen name = this.ValveState[name]
    member this.Open (rooms: Map<string, Room>) name =     
        if not this.ValveState[name] && rooms[name].FlowRate > 0 then
            true, { this with ValveState = this.ValveState |> Map.add name true }
        else 
            false, this
    member this.AllOpen (rooms: Map<string, Room>) =
        this.ValveState 
        |> Map.toSeq 
        |> Seq.forall (fun (valve, isOpen) -> 
            isOpen || rooms[valve].FlowRate = 0)

    member this.Identity = { MyRoomName = this.MyRoom; ElephantsRoomName = this.ElephantsRoom; Time = this.Time; TotalFlow = this.TotalFlow }

module RoomState2 = 
    let openValve (rooms: Map<string, Room>) name state =
        if not state.ValveState[name] && rooms[name].FlowRate > 0 then 
            true, { state with ValveState = state.ValveState |> Map.add name true }
        else
            false, state

let isSolution2 state = state.Time = TTL2

let neighbours2 (rooms: Map<string, Room>) (roomState: RoomState2) =
    
    if roomState.AllOpen rooms then
        [{ roomState with Time = roomState.Time + 1; Parent = Some(roomState) }]
    else 
        let removeEquivalent candidates =
            candidates
             |> List.fold(fun acc (a, b) -> 
                if acc |> Set.contains (b, a) then
                    acc
                else
                    acc |> Set.add (a,b)
            ) Set.empty
            |> Set.toList

        let possibleAdjacentRooms =
            [
                for myRoom in rooms[roomState.MyRoom].TunnelTo do
                    for elephantsRoom in rooms[roomState.ElephantsRoom].TunnelTo do
                        yield myRoom, elephantsRoom
            ]
        
        let nextTime = roomState.Time + 1

        let adjacentRooms =
            if roomState.MyRoom = roomState.ElephantsRoom then
                possibleAdjacentRooms |> removeEquivalent 
            else
                possibleAdjacentRooms
            |> List.map (fun (myRoom, elephantsRoom) -> 
                { 
                    roomState with
                        MyRoom = myRoom
                        ElephantsRoom = elephantsRoom
                        Time = nextTime
                        Parent = Some(roomState)
                }
            )        

        let onlyMyOpened, onlyMyOpenedState = roomState.Open rooms roomState.MyRoom
        let onlyElephantOpened, onlyElephantOpenedState = roomState.Open rooms roomState.ElephantsRoom
        let bothOpened, bothOpenedState = onlyMyOpenedState.Open rooms roomState.ElephantsRoom

        let timeFactor = TTL2 - nextTime
        let myFlowRate = rooms[roomState.MyRoom].FlowRate
        let elephantsFlowRate = rooms[roomState.ElephantsRoom].FlowRate

        let onlyMineOpenedStates = 
            if onlyMyOpened then
                rooms[roomState.ElephantsRoom].TunnelTo
                |> List.map (fun r -> { 
                    onlyMyOpenedState with 
                        ElephantsRoom = r
                        Time = nextTime
                        TotalFlow = roomState.TotalFlow + myFlowRate*timeFactor
                        Parent = Some(roomState)})
            else 
                []

        let onlyElephantsOpenedStates =
            if onlyElephantOpened then
                rooms[roomState.MyRoom].TunnelTo
                |> List.map (fun r -> { 
                    onlyElephantOpenedState with 
                        MyRoom = r
                        Time = nextTime
                        TotalFlow = roomState.TotalFlow + elephantsFlowRate*timeFactor
                        Parent = Some(roomState) })
            else 
                []

        let bothOpenedStates = 
            if onlyMyOpened && bothOpened then
                [{ 
                    bothOpenedState with 
                        Time = roomState.Time + 1
                        TotalFlow = roomState.TotalFlow + (myFlowRate + elephantsFlowRate)*timeFactor
                        Parent = Some(roomState)
                }]
            else 
                []

        onlyMineOpenedStates @ onlyElephantsOpenedStates @ bothOpenedStates @ adjacentRooms
    
let roomIdentity2 (room: RoomState2) = room.Identity

let nonVisitedNeighbours2 (toNodeIdentity: nodeIdentity<RoomState2, RoomIdentity2>) rooms (visited:HashSet<RoomIdentity2>) room =
    neighbours2 rooms room
    |> List.filter (fun neighbour -> visited.Add(neighbour |> toNodeIdentity))

let bfs2 isSolution nonVisitedNeighbours startState =
    let visited = new HashSet<RoomIdentity2>()
    let queue = new Queue<RoomState2*int>()
    let rec bfsSearch() =
        if queue.Count = 0 then
            failwith "no solution found"
        else
            let (head, depth) = queue.Dequeue()
            if isSolution head then 
                queue
            else 
                for neighbour in nonVisitedNeighbours visited head do
                    queue.Enqueue (neighbour, depth+1)
                bfsSearch()
    
    queue.Enqueue (startState,0)
    
    bfsSearch()

let openedUp2 roomState =
    roomState.ValveState
    |> Map.toSeq
    |> Seq.filter(fun (valve, isOpen) -> 
        match roomState.Parent with
        | Some state -> isOpen && (not state.ValveState[valve])
        | None -> isOpen)
    |> Seq.toList

let rec print2 rooms (roomState: RoomState2) =
    let opened = roomState |> openedUp2
    let timeLeft = TTL2 - roomState.Time
    printfn "%2i (%2i): You're at: %s, Elephant at: %s Total Flow: %i Opened: %A (%b)" 
        roomState.Time
        timeLeft
        roomState.MyRoom 
        roomState.ElephantsRoom 
        roomState.TotalFlow 
        opened 
        (roomState.AllOpen rooms)


    match roomState.Parent with
    | Some state -> print2 rooms state
    | None -> printfn "Back to where we started"

let secondStar = 
    let rooms = input |> parse
    let valveState = rooms |> initialValveState
    let startRoom = { MyRoom = "AA"; ElephantsRoom = "AA"; ValveState = valveState; Time = 0; TotalFlow = 0; Parent = None }

    let queue = bfs2 isSolution2 (nonVisitedNeighbours2 roomIdentity2 rooms) startRoom
    
    queue 
    |> Seq.toList
    |> Seq.map fst
    |> Seq.filter (fun state -> state.Time = TTL2)    
    |> Seq.sortBy (fun state -> state.TotalFlow)
    |> Seq.last
    // |> print2 rooms
    |> (fun state -> state.TotalFlow)

secondStar

20*(TTL2+1-3)+21*(TTL2+1-4)+(13+22)*(TTL2+1-8)+2*(TTL2+1-10)+3*(TTL2+1-12)=1707

20*24+23*(21+20)