// --- Day 16: Proboscidea Volcanium ---

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

let exampleRooms = example |> parse
let rooms = input |> parse

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
        { room with 
            ValveState = room.ValveState |> Map.add room.Name true
            Time = room.Time + 1
            TotalFlow = room.TotalFlow + rooms[room.Name].FlowRate
        }::adjacentRooms
    else
        adjacentRooms


let exampleValveState = exampleRooms |> initialValveState

let exampleStartRoom = { Name = "AA"; ValveState = exampleValveState; Time = 0; TotalFlow = 0; Parent = None }

neighbours exampleRooms exampleStartRoom

type nodeIdentity<'node,'identity> = 'node -> 'identity



let roomIdentity (room: RoomState) = room.Identity
let exampleStartVisited = exampleStartRoom |> roomIdentity |> Set.singleton



type NodeSet<'node when 'node : comparison> = Set<'node>

// from day 12:
let shortestPath neighbours startExploring destination = 

    let neighboursNotVisited (visited: NodeSet<'node>) nodeSet =
        neighbours nodeSet
        |> List.filter(visited.Contains >> not) 
        |> Set

    Seq.unfold(fun ((exploring, visited): NodeSet<'node>*NodeSet<'node>) -> 
        if visited.IsSupersetOf destination then 
            None 
        else
            let newExploring = exploring |> Seq.map (neighboursNotVisited visited) |> Set.unionMany
            let newVisited = visited + newExploring
            let newState = newExploring, newVisited
            Some(newState, newState)) (startExploring, startExploring)
    |> Seq.length

let n1 = neighbours exampleRooms exampleStartRoom
let n2 = neighbours exampleRooms n1[0]
let n3 = neighbours exampleRooms n2[0]



let firstStar =
    0

firstStar

let secondStar = 
    0

secondStar

