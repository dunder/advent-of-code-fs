module AoC.E2020.Day05

open AoC
open IO

// --- Day 5: Binary Boarding ---

let input = readInputLines "2020" "Day05" |> List.ofSeq

let parse rows columns (spec:string) =
    let rowSpec = spec.[0..6]
    let columnSpec = spec.[7..9]

    let row = 
        rowSpec
        |> Seq.fold (fun acc n ->
            let parts = acc |> List.splitInto 2
            match n with 
            | 'F' -> parts |> List.head
            | 'B' -> parts |> List.last
            | _ -> failwithf "Unmatched specification: %s %c"  rowSpec n
        ) [ 0..rows-1 ]

    let column = 
        columnSpec
        |> Seq.fold (fun acc n ->
            let parts = acc |> List.splitInto 2
            match n with 
            | 'L' -> parts |> List.head
            | 'R' -> parts |> List.last
            | _ -> failwithf "Unmatched specification: %s %c"  rowSpec n
        ) [ 0..columns-1 ]
    
    (row.Head, column.Head)

let seatId (row, column) =
    row * 8 + column

let firstStar () =
    
    input |> List.map (parse 128 8) |> List.map seatId |> List.max

    
let secondStar () = 
    
    let seats = input |> List.map (parse 128 8)

    let seatIds = seats |> List.map (fun s -> s |> seatId) |> Set.ofList

    let nonFullRows = 
        seats 
        |> List.groupBy fst 
        |> Seq.filter (fun s -> s |> snd |> List.length < 8)

    let emptySeats = 
        nonFullRows
        |> Seq.map snd
        |> Seq.map (fun seats ->
            let row = seats |> List.head |> fst
            let allSeatsInARow = [0..7] |> Set.ofList
            let occupied = seats |> List.map snd |> Set.ofList
            let available = Set.difference allSeatsInARow occupied
            (row, available |> Seq.exactlyOne)
        )

    let mySeat = 
        emptySeats 
        |> Seq.filter (fun seat -> 
            let seatId' = seat |> seatId
            seatIds |> Set.contains (seatId' + 1)
            &&
            seatIds |> Set.contains (seatId' - 1)
        )
        |> Seq.exactlyOne

    mySeat |> seatId


module Tests =

    open Xunit

    [<Fact>]
    let ``first star`` () =
        Assert.Equal(855, firstStar())

    [<Fact>]
    let ``second star`` () =
        Assert.Equal(552, secondStar())
        


