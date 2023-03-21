
module AoC.E2022.Day16

// --- Day 16: Proboscidea Volcanium ---

open AoC
open IO

open System.Collections.Generic
open System.Text.RegularExpressions


let input = readInputLines "2022" "Day16" |> List.ofSeq

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


let firstStar () =
    
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
    |> (fun state -> state.TotalFlow)

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

    member this.Identity = { MyRoomName = this.MyRoom; ElephantsRoomName = this.ElephantsRoom; Time = this.Time; TotalFlow = this.TotalFlow }

module RoomState2 = 
    let openValve (rooms: Map<string, Room>) name state =
        if not state.ValveState[name] && rooms[name].FlowRate > 0 then 
            true, { state with ValveState = state.ValveState |> Map.add name true }
        else
            false, state

let isSolution2 state = state.Time = TTL2

let neighbours2 (rooms: Map<string, Room>) (roomState: RoomState2) =
    
    let adjacentRooms = 
        [
            for myRoom in rooms[roomState.MyRoom].TunnelTo do
                for elephantsRoom in rooms[roomState.ElephantsRoom].TunnelTo do
                    yield { 
                        MyRoom = myRoom
                        ElephantsRoom = elephantsRoom
                        ValveState = roomState.ValveState
                        Time = roomState.Time + 1
                        TotalFlow = roomState.TotalFlow
                        Parent = Some(roomState) }
        ]

    // optimize AB vs BA?

    let nextTime = roomState.Time + 1


    let iOpened, myState = roomState.Open rooms roomState.MyRoom
    let elephantOpened, elephantState = myState.Open rooms roomState.ElephantsRoom

    let timeFactor = (TTL2 - nextTime)
    let myAddedFlow = if iOpened then rooms[roomState.MyRoom].FlowRate else 0
    let elephantsAddedFlow = if elephantOpened then rooms[roomState.ElephantsRoom].FlowRate else 0
    let newFlowRate = roomState.TotalFlow + timeFactor*(myAddedFlow + elephantsAddedFlow)

    if iOpened || elephantOpened then
        { elephantState with Time = nextTime; TotalFlow = newFlowRate}::adjacentRooms
    else
        adjacentRooms

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

let secondStar () = 
    let rooms = example |> parse
    let valveState = rooms |> initialValveState
    let startRoom = { MyRoom = "AA"; ElephantsRoom = "AA"; ValveState = valveState; Time = 0; TotalFlow = 0; Parent = None }

    let queue = bfs2 isSolution2 (nonVisitedNeighbours2 roomIdentity2 rooms) startRoom
    
    queue 
    |> Seq.toList
    |> Seq.map fst
    |> Seq.filter (fun state -> state.Time = TTL2)    
    |> Seq.sortBy (fun state -> state.TotalFlow)
    |> Seq.last
    |> (fun state -> state.TotalFlow)