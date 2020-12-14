module AoC.E2020.Day14

open AoC
open IO
open System
open System.Text.RegularExpressions

// --- Day 14: Docking Data ---

let input = readInputLines "2020" "Day14" |> List.ofSeq

type Instruction = { address: int64; value: int64 }
type Program = { bitmask: string; instructions: list<Instruction> }

let parse (lines: list<string>) = 
    
    lines
    |> List.mapFold (fun acc line -> 
       if line.StartsWith("mask") then
            ((acc + 1,line), acc + 1)
       else
            ((acc,line), acc)
    ) 0
    |> fst
    |> List.groupBy (fun (i, _) -> i)

let toInstruction (instruction: string) =
    let regex = new Regex("mem\[(\d+)\] = (.+)")
    let m = regex.Match(instruction)
    let address = m.Groups.[1].Value |> int64
    let value = m.Groups.[2].Value |> int64
    { address = address; value = value  }

let toProgram (instructionGroups: list<list<string>>) =
    instructionGroups
    |> List.map (fun instructionGroup ->
        let maskLine = instructionGroup |> List.head
        let mask = maskLine.Split(" = ") |> Array.last

        let instructions = 
            instructionGroup 
            |> List.tail
            |> List.map toInstruction

        { bitmask = mask; instructions = instructions }
    )
    
let maskToSet (mask: string) =
    mask.Replace("X", "0")

let maskToClear (mask: string) = 
    mask.Replace("X", "1")

let set (mask: string) (value: int64) =
    let maskForSet = maskToSet mask 
    let maskValue = Convert.ToInt64(maskForSet, 2)
    
    let result = maskValue ||| value

    let maskForClear = maskToClear mask
    let maskValueForClear = Convert.ToInt64(maskForClear, 2)

    let next = maskValueForClear &&& result

    next

let generateAddresses (mask: string) (address: int64) =
    let maskForSet = maskToSet mask 
    let maskValue = Convert.ToInt64(maskForSet, 2)
    
    let result = maskValue ||| address

    let modifiedMask = 
        Convert.ToString(result, 2).PadLeft(36, '0')
        |> String.mapi (fun i c -> if mask.[i] = 'X' then 'X' else c)

    let idxs = mask |> Seq.mapi (fun i c -> i, c) |> Seq.filter (fun (i,c) -> c = 'X') |> Seq.map fst
    let xs = idxs |> Seq.length
    let times = xs |> float

    let y = [0..(2.0**times |> int)-1] |> List.map (fun x -> Convert.ToString(x, 2).PadLeft(xs, '0'))

    y
    |> List.map (fun binary -> 
        let z = binary |> Seq.zip idxs |> Map.ofSeq
        modifiedMask |> String.mapi (fun i c -> 
            if c = 'X' then 
                z.[i]
            else
                c
        )
    )
    |> List.ofSeq
    |> List.map (fun binary -> Convert.ToInt64(binary, 2))

let evaluate (program: Program) (state: Map<int64, int64>) =
    program.instructions
    |> List.fold (fun (map: Map<int64, int64>) instruction -> 
        let value = set program.bitmask instruction.value
        map |> Map.add instruction.address value
    ) state

let evaluate2 (program: Program) (state: Map<int64, int64>) =
    program.instructions
    |> List.fold (fun (map: Map<int64, int64>) instruction -> 
        let addresses = generateAddresses program.bitmask instruction.address
        addresses
            |> List.fold(fun m address -> 
                m |> Map.add address instruction.value
            ) map
    ) state


let firstStar () =

    let x = 
        parse input 
        |> List.map snd
        |> List.map (fun x -> x |> List.map snd)
        |> toProgram

    let memory = 
        x
        |> Seq.fold (fun (state: Map<int64,int64>) program -> 
            evaluate program state
        ) Map.empty
    
    let result =
        memory 
        |> List.ofSeq
        |> List.sumBy (fun x -> x.Value)

    result

let secondStar () = 
    let x = 
        parse input 
        |> List.map snd
        |> List.map (fun x -> x |> List.map snd)
        |> toProgram

    let memory = 
        x
        |> Seq.fold (fun (state: Map<int64,int64>) program -> 
            evaluate2 program state
        ) Map.empty

    let result =
        memory 
        |> List.ofSeq
        |> List.sumBy (fun x -> x.Value)

    result


module Tests =

    open Xunit

    [<Fact>]
    let ``first star`` () =
        Assert.Equal(5902420735773L, firstStar())

    [<Fact>]
    let ``second star`` () =
        Assert.Equal(3801988250775L, secondStar())
        


