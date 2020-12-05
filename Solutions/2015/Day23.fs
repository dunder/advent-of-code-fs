module AoC.E2015.Day23

open AoC
open IO
open ActivePatterns

// --- Day 23: Opening the Turing Lock ---

let input = readInputLines "2015" "Day23" |> List.ofSeq

type ExecutionState = { Instruction: int; Registers: Map<char,int>}

type Instruction = 
    | Half of char
    | Triple of char
    | Increment of char
    | Jump of int
    | JumpEven of char*int
    | JumpOne of char*int

let parseOffset sign offset =
    if sign = "-" then
        sign + offset |> int
    else 
        offset |> int

let parse lines = 
    lines
    |> List.mapi (fun i line -> 
        let instr =
            match line with
            | Regex "hlf (a|b)" [register] -> Half (register |> char)
            | Regex "tpl (a|b)" [register] -> Triple (register |> char)
            | Regex "inc (a|b)" [register] -> Increment (register |> char)
            | Regex "jmp (-|\+)(\d+)" [sign; offset] -> Jump (parseOffset sign offset)
            | Regex "jie (a|b), (-|\+)(\d+)" [register; sign; offset] -> JumpEven ((register |> char), (parseOffset sign offset))
            | Regex "jio (a|b), (-|\+)(\d+)" [register; sign; offset] -> JumpOne ((register |> char), (parseOffset sign offset))
            | _ -> failwithf "Unrecognized instruction: %s" line
        (i, instr)
    )

let jump offset state = 
    { state with Instruction = state.Instruction + offset }

let update register (transform:int->int) state = 
    let newValue = transform state.Registers.[register] 
    { state with Registers = state.Registers |> Map.add register newValue }

let check register (predicate: int->bool) state =
    predicate state.Registers.[register]

let half register state = 
    state |> (update register (fun r -> r / 2) >> jump 1)

let triple register state =
    state |> (update register ((*) 3) >> jump 1)

let increment register state = 
    state |> (update register ((+) 1) >> jump 1)

let jumpEven register offset state =
    if check register (fun r -> r % 2 = 0) state then
        state |> jump offset
    else
        state |> jump 1

let jumpOne register offset state =
    if check register (fun r -> r = 1) state then
        state |> jump offset
    else
        state |> jump 1

let evaluate instruction (state:ExecutionState) = 
    let operation = 
        match instruction with 
        | Half register -> half register
        | Triple register -> triple register
        | Increment register -> increment register
        | Jump offset -> jump offset
        | JumpEven (register, offset) -> jumpEven register offset
        | JumpOne (register, offset) -> jumpOne register offset
    state |> operation

let execute registers (program:list<int*Instruction>) =
    let finalState = 
        { Instruction = 0; Registers = registers }
        |> Seq.unfold (fun state -> 
            if state.Instruction < 0 || state.Instruction >= program.Length then
                None
            else
                let instruction = program.[state.Instruction]
                let nextState = state |> evaluate (snd instruction)
                Some (state, nextState)
        )
        |> Seq.last

    finalState.Registers.['b']

let firstStar () =

    parse input |> execute ([('a', 0);('b', 0)] |> Map.ofSeq)

    
let secondStar () = 
    
    parse input |> execute ([('a', 1);('b', 0)] |> Map.ofSeq)


module Tests =

    open Xunit

    [<Fact>]
    let ``first star`` () =
        Assert.Equal(184, firstStar())

    [<Fact>]
    let ``second star`` () =
        Assert.Equal(231, secondStar())