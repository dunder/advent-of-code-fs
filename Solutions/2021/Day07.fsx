// --- Day 7: The Treachery of Whales ---

open System.IO

System.Environment.CurrentDirectory <- __SOURCE_DIRECTORY__

let input = File.ReadAllText(@".\Input\Day07.txt")

let example = "16,1,2,0,4,2,7,1,2,14"

let parse (text:string) = 
    text.Split(",")
    |> Array.map System.Int32.Parse
    |> Array.toList

let distance a b = abs (a-b)

let fuel positions target = 
    positions 
    |> Seq.map (fun position -> distance position target) 
    |> Seq.sum

let firstStar =
    let data = input
    let positions = parse data
    
    seq {
            for x in 1..System.Int32.MaxValue do
                let cost = fuel positions x
                printfn ">>> %i" cost
                yield fuel positions x
    }
    |> Seq.take 1000
    |> Seq.min


firstStar

let fuelDistance (x:int) (y:int) =
    let distance = abs (x - y)
    [0..distance]
    |> Seq.reduce (fun state x -> x + state)

fuelDistance 16 5

let fuelCost positions target = 
    positions 
    |> Seq.map (fun position -> fuelDistance position target) 
    |> Seq.sum

let secondStar = 
    let data = input
    let positions = parse data    

    seq {
            for x in 1..System.Int32.MaxValue do
                let cost = fuelCost positions x
                printfn ">>> %i" cost
                yield cost
    }
    |> Seq.take 1000
    |> Seq.min


secondStar

