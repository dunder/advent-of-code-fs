module AoC.E2016.Day01

open AoC
open IO
open Matrix

// --- Day 1: No Time for a Taxicab ---

let input = readInputText "2016" "Day01"

let toTurn = function
    | 'L' -> Left
    | 'R' -> Right
    | c -> failwithf "Unrecognized turn %c" c

let visits facing steps pos =
    [1..steps] |> List.scan (fun acc _ -> (move facing acc)) pos |> List.tail

let firstStar () =
    let finish = 
        input.Split(", ")
        |> Seq.fold (fun (facing, position) direction -> 
            let turnTo = direction |> Seq.head |> toTurn
            let steps = direction |> Seq.tail |> System.String.Concat |> int
            let nowFacing = turn facing turnTo
            let newPosition = position |> moveN steps nowFacing
            (nowFacing, newPosition)
        ) (Direction.North, { X = 0; Y = 0 })

    manhattanDistance { X = 0; Y = 0 } (finish |> snd)
    

let secondStar () =
    input.Split(", ")
    |> Seq.scan (fun (facing, (position, _)) direction -> 
        let turnTo = direction |> Seq.head |> toTurn
        let steps = direction |> Seq.tail |> System.String.Concat |> int
        let nowFacing = turn facing turnTo
        let positions = visits nowFacing steps position
        (nowFacing, (positions |> List.last, positions))
    ) (Direction.North, ({ X = 0; Y = 0 }, []))
    |> Seq.map (snd >> snd)
    |> Seq.collect (id)
    |> Seq.mapFold (fun (visited:Map<Position,int>) pos -> 
        let count = if visited.ContainsKey pos then visited.[pos] + 1 else 1
        ((pos, count), visited |> Map.add pos count)
    ) Map.empty
    |> fst
    |> Seq.find (fun (_, count) -> count > 1)
    |> fst
    |> manhattanDistance { X = 0; Y = 0 }
    
        


module Tests =

    open Xunit

    [<Fact>]
    let ``first star`` () =

        Assert.Equal(209, firstStar())

    [<Fact>]
    let ``second star`` () =

        Assert.Equal(136, secondStar())
