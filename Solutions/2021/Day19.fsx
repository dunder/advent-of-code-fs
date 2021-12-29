// --- Day 19: Beacon Scanner ---

open System.IO
open System.Text.RegularExpressions

System.Environment.CurrentDirectory <- __SOURCE_DIRECTORY__

let input = File.ReadLines(@".\Input\Day19.txt") |> List.ofSeq
let example = File.ReadLines(@".\Input\Day19-example.txt") |> List.ofSeq

let (|Regex|_|) pattern input = 
    let m = Regex.Match(input, pattern)

    if m.Success
    then Some(List.tail [for g in m.Groups -> g.Value])
    else None

let parse lines = 
    lines
    |> Seq.fold (fun (state: list<list<int*int*int>>) line -> 
        match line with
        | Regex @"(-?\d+),(-?\d+),(-?\d+)" [x; y; z] -> 
            let current = state.Head
            ((x |> int, y |> int, z |> int)::current)::state.Tail
        | Regex @"--- scanner \d+ ---" [] -> 
            []::state
        | _ -> state
    ) []
    |> List.map List.rev
    |> List.rev

let scramble pos = 
    let x,y,z = pos
    seq {
        yield x,y,z
        yield y,x,z
        yield y,z,x
        yield x,z,y
        yield z,x,y
        yield z,y,x
    }

let flip pos =
    let x, y, z = pos
    seq {
        yield x,y,z
        yield -x,y,z
        yield x,-y,z
        yield x,y,-z
        yield -x,-y,z
        yield x,-y,-z
        yield -x,y,-z
        yield -x,-y,-z
    }

flip (5,6,-4) 
|> Seq.map scramble
|> Seq.collect id
|> Seq.toList
|> List.distinct
|> List.sort
|> List.length

let allOrientations pos =
    pos
    |> flip
    |> Seq.map scramble
    |> Seq.collect id
    |> Seq.toList

let diffScanners (scanner1:list<int*int*int>) (scanner2:list<int*int*int>) =

    let diff pos1 pos2 = 
        let x1, y1, z1 = pos1
        let x2, y2, z2 = pos2

        x2-x1, y2-y1, z2-z1

    seq {
        for i in 0..scanner1.Length-1 do
            for j in 0..scanner2.Length-1 do
                let pos1 = scanner1.[i]
                let pos2 = scanner2.[j]
                let pos2Orientations = allOrientations pos2
                pos1, pos2, pos2Orientations |> List.map (fun pos -> diff pos pos1)
    }
    |> List.ofSeq

let project (scanner:list<int*int*int>) i relative = 
    scanner
    |> List.map (fun pos ->
        let x1, y1, z1 = (allOrientations pos).[i]
        let x2, y2, z2 = relative
        x1 + x2, y1 + y2, z1 + z2
    )

let matchScanners (scanner1:list<int*int*int>) (scanner2:list<int*int*int>) =

    let scannerDiff = diffScanners scanner1 scanner2

    let grouped = 
        [0..47]
        |> List.map (fun i -> 
            let matched = 
                scannerDiff 
                |> List.groupBy (fun (pos1, pos2, diffs) -> 
                    // sometimes diffs length is 24 and other times 48 cant handle 24
                    diffs.[i])
                |> List.filter (fun (key, group) -> group.Length >= 12)
            match matched |> Seq.length with
            | 1 -> 
                let diff, matching = matched |> Seq.exactlyOne
                Some (i, matching |> List.map (fun (pos1, pos2, _ )-> pos1, pos2 ))
            | 0 -> None
            | n -> failwithf "Expected one or no match but found %i matches" n
        )
        |> List.choose id

    let diff pos1 pos2 = 
        let x1, y1, z1 = pos1
        let x2, y2, z2 = pos2

        x1-x2, y1-y2, z1-z2
    
    if grouped |> Seq.length = 1 then
        let i, matchedPositions = (grouped |> Seq.exactlyOne)
        let pos1, pos2 = matchedPositions.[0]
        let pos2 = (allOrientations pos2).[i]
        let reference = diff pos1 pos2
        Some (i, reference, matchedPositions |> List.map (fun (pos1, pos2) -> pos1, (allOrientations pos2).[i]), project scanner2 i reference)
    else 
        None

type ScannerSearch = 
    { 
        Overlap: Set<int*int*int>
        ScannersToMatch: list<int>
        VisitedScanners: Set<int> 
        ProjectedReadings: Map<int,list<int*int*int>>
        ScannerPositions: Map<int, int*int*int>
        Scanners: list<list<int*int*int>>
    }

module ScannerSearch = 

    let otherScanners scannerSearch = 
        scannerSearch.Scanners 
        |> List.indexed 
        |> List.filter (fun (i, scanner) -> not (scannerSearch.VisitedScanners |> Set.contains i))

    let countBeacons scannerSearch = 
        scannerSearch.ProjectedReadings
        |> Map.toList
        |> List.map snd
        |> List.collect id
        |> Set.ofList
        |> Set.count

let scannerSearch (scannerReadings: list<list<int*int*int>>) = 

    let searchState = 
        { 
            Overlap = Set.empty
            ScannersToMatch = [0]
            VisitedScanners = Set.empty
            ProjectedReadings = Map.empty |> Map.add 0 scannerReadings.[0]
            ScannerPositions = Map.empty |> Map.add 0 (0,0,0)
            Scanners = scannerReadings}

    Seq.unfold(fun searchState ->

        let nextScannerToEvaluate = searchState.ScannersToMatch.Head
        let newVisitedScanners = searchState.VisitedScanners |> Set.add nextScannerToEvaluate
        let searchState = { searchState with VisitedScanners = newVisitedScanners }
        let otherScanners = searchState |> ScannerSearch.otherScanners
        let matchedScanners = otherScanners |> List.map (fun (i, scanner) -> i, matchScanners searchState.ProjectedReadings.[nextScannerToEvaluate] scanner)

        let matchingScanners = 
            matchedScanners 
            |> List.filter (fun (_, matched) ->
                match matched with
                | Some _ -> true
                | None -> false
            )

        let matchingScannerIndeces = matchingScanners |> List.map fst

        let searchState = 
            matchingScanners
            |> Seq.fold (fun searchState (matchedScannerIndex, matchedScanner) -> 

                match matchedScanner with
                | Some matchedScanner ->
                    let orientationIndex, relative, matchedBeacons, projectedReadings = matchedScanner
                    let newPositions = searchState.ScannerPositions |> Map.add matchedScannerIndex relative
                    let newBeacons = matchedBeacons |> List.map fst |> Set.ofList
                    let newOverlap = searchState.Overlap |> Set.union newBeacons
                    let newProjectedReadings = searchState.ProjectedReadings |> Map.add matchedScannerIndex projectedReadings
                
                    { searchState with Overlap = newOverlap; ProjectedReadings = newProjectedReadings; ScannerPositions = newPositions }
                | None -> searchState
            
            ) searchState

        let newScannersToMatch = matchingScannerIndeces @ searchState.ScannersToMatch.Tail

        let nextSearchState = { searchState with  ScannersToMatch = newScannersToMatch }

        Some(nextSearchState, nextSearchState)
    ) searchState
    |> Seq.find (fun searchState -> searchState.ScannersToMatch |> List.isEmpty)


let firstStar =
    let scanners = parse input

    let searchResult = scannerSearch scanners

    searchResult |> ScannerSearch.countBeacons

// returns a list of lists of all the combinations of n of the list of l
let rec combinations n l = 
    match n, l with
    | 0, _ -> [[]]
    | _, [] -> []
    | k, (x::xs) -> List.map ((@) [x]) (combinations (k-1) xs) @ combinations k xs

let manhattanDistance (x, y, z) (x', y', z') = abs (x' - x) + abs (y' - y) + abs (z' - z)

let secondStar =
    let scanners = parse input

    let searchResult = scannerSearch scanners

    let scannerPositions = searchResult.ScannerPositions |> Map.toList |> List.map snd

    combinations 2 scannerPositions 
    |> List.map (fun pair -> manhattanDistance pair.[0] pair.[1])
    |> List.max

    


