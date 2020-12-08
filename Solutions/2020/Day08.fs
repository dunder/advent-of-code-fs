module AoC.E2020.Day08

open AoC
open IO
open ActivePatterns

// --- Day 8: Handheld Halting ---

let input = readInputLines "2020" "Day08" |> List.ofSeq


type ExecutionState = { Instruction: int; Accumulator: int; ExecutedInstructions: Set<int> }

type Instruction = 
    | Acc of int
    | Jump of int
    | Nop of int

let parseWithSign sign value =
    if sign = "-" then
        sign + value |> int
    else 
        value |> int

let parse lines = 
    lines
    |> List.mapi (fun i line -> 
        let instr =
            match line with
            | Regex "acc (-|\+)(\d+)" [sign; count] -> Acc (parseWithSign sign count)
            | Regex "jmp (-|\+)(\d+)" [sign; offset] -> Jump (parseWithSign sign offset)
            | Regex "nop (-|\+)(\d+)" [sign; offset] -> Nop (parseWithSign sign offset)
            | _ -> failwithf "Unrecognized instruction: %s" line
        (i, instr)
    )

let jump offset state = 
    { state with Instruction = state.Instruction + offset }

let acc count state = 
    { state with Accumulator = state.Accumulator + count } |> jump 1

let updateExecuted state = 
    { state with ExecutedInstructions = state.ExecutedInstructions |> Set.add state.Instruction}

let evaluate instruction (state:ExecutionState) = 
    let operation = 
        match instruction with 
        | Acc count -> acc count
        | Jump offset -> jump offset
        | Nop _ -> jump 1
    state |> operation

let execute (program:list<int*Instruction>) =
        
    { Instruction = 0; Accumulator = 0; ExecutedInstructions = Set.empty }
    |> Seq.unfold (fun state -> 
        if state.ExecutedInstructions.Contains(state.Instruction) || state.Instruction >= program.Length then
            None
        else
            let instruction = program.[state.Instruction]
            let state = state |> updateExecuted
            let nextState = state |> evaluate (snd instruction)
            Some (state, nextState)
    )
    |> Seq.last


let firstStar () =
    let finalState =
        parse input
        |> execute
    finalState.Accumulator

let secondStar () = 
    let program = parse input
    let swappable = program |> Seq.filter (fun (_, instr) ->
        match instr with
        | Jump _
        | Nop _ -> true
        | Acc _ -> false
    )
    let result = 
        swappable |> Seq.map (fun (i, instr) ->
            let firstPart = program |> List.take i
            let secondPart = program |> List.skip (i + 1)
            let swappedInstruction = 
                match instr with
                | Nop offset -> Jump (offset)
                | Jump offset -> Nop (offset)
                | _ -> failwithf "Should not swap: %O" instr
            let newProgram = firstPart @ (i, swappedInstruction)::secondPart
            newProgram |> execute
        )
        |> Seq.find (fun finalState -> finalState.Instruction >= program.Length-1)
    
    result.Accumulator


module Tests =

    open Xunit

    [<Fact>]
    let ``first star`` () =
        Assert.Equal(1782, firstStar())

    [<Fact>]
    let ``second star`` () =
        Assert.Equal(797, secondStar())
        


