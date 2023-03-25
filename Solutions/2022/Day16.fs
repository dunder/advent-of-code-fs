
module AoC.E2022.Day16

// --- Day 16: Proboscidea Volcanium ---

open AoC
open IO

open System.Collections.Generic
open System.Text.RegularExpressions


let input = readInputLines "2022" "Day16" |> List.ofSeq

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

let neighbours ttl (rooms: Map<string, Room>) (room: RoomState) =
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
                TotalFlow = room.TotalFlow + rooms[room.Name].FlowRate*(ttl-nextTime)
            }::adjacentRooms
        else
            adjacentRooms

type nodeIdentity<'node,'identity> = 'node -> 'identity
let roomIdentity (room: RoomState) = room.Identity

let nonVisitedNeighbours ttl (toNodeIdentity: nodeIdentity<RoomState, RoomIdentity>) rooms (visited:HashSet<RoomIdentity>) room =
    neighbours ttl rooms room
    |> List.filter (fun neighbour -> visited.Add(neighbour |> toNodeIdentity))

let isSolution ttl state = state.Time = ttl

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

let rec print ttl rooms (roomState: RoomState) =
    let time = ttl - roomState.Time + 1
    let opened = roomState |> openedUp
    printfn "%2i: %s Total Flow: %i Opened: %A All opened: %b" 
        time
        roomState.Name 
        roomState.TotalFlow
        opened
        (roomState.AllOpen rooms)
    match roomState.Parent with
    | Some state -> print ttl rooms state
    | None -> printfn "Back to where we started"

let mostPressure ttl valveState = 
    let rooms = input |> parse    
    let startRoom = { Name = "AA"; ValveState = valveState; Time = 0; TotalFlow = 0; Parent = None }

    let queue = bfs (isSolution ttl) (nonVisitedNeighbours ttl roomIdentity rooms) startRoom
    
    queue 
    |> Seq.toList
    |> Seq.map fst
    |> Seq.filter (fun state -> state.Time = ttl)    
    |> Seq.sortBy (fun state -> state.TotalFlow)
    |> Seq.last

let firstStar() =

    let rooms = input |> parse
    let valveState = rooms |> initialValveState

    let endState = mostPressure 30 valveState

    endState.TotalFlow

let secondStar() =
    
    let ttl = 26
    let rooms = input |> parse
    let valveState = rooms |> initialValveState
    let firstRun = mostPressure ttl valveState 
    let secondRun = mostPressure ttl firstRun.ValveState

    firstRun.TotalFlow + secondRun.TotalFlow
