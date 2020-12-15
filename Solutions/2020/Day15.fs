module AoC.E2020.Day15

open AoC

// --- Day 15: Rambunctious Recitation ---

let input = [16;11;15;0;1;7]

type ShortHistory<'a> = { Last: option<'a>; Previous: option<'a> }

module ShortHistory = 
    
    let add value history =
        let prev = history.Last
        let last = Some(value)
        { Last = last; Previous = prev }

    let previousOrLast history = 
        match history.Previous with
        | Some _ -> history.Previous
        | None -> history.Last
    
type GameState = { 
    Round: int
    LastSpoken: int
    SpokenRounds: Map<int, ShortHistory<int>>
    Numbers: list<int>
}

module GameState = 

    let isLastSpokenBefore (state: GameState) =
        state.SpokenRounds |> Map.containsKey state.LastSpoken

    let next (state: GameState) = 
        let speak = 
            if state.Round < state.Numbers.Length then
                state.Numbers.[state.Round]
            else
                if not <| isLastSpokenBefore state then
                   0
                else
                    let previous = (state.SpokenRounds.[state.LastSpoken] |> ShortHistory.previousOrLast)
                    state.Round - 1 - previous.Value

        let newShortHistory = 
            if state.SpokenRounds |> Map.containsKey speak then
                state.SpokenRounds.[speak] |> ShortHistory.add state.Round
            else
                { Last = Some(state.Round); Previous = None }

        let newHistory = state.SpokenRounds |> Map.add speak newShortHistory

        { state with
            Round = state.Round + 1
            LastSpoken = speak
            SpokenRounds = newHistory
        }

            
let runTo numbers round = 
    let initial = { 
        Round = 0
        LastSpoken = 0
        SpokenRounds = Map.empty
        Numbers = numbers
    }

    let finalState = [0..round-1] |> Seq.fold (fun state _ -> state |> GameState.next ) initial

    finalState.LastSpoken

let firstStar () =
    
    runTo input 2020
    

let secondStar () = 
    
    runTo input 30000000


module Tests =

    open Xunit

    [<Fact>]
    let ``first star`` () =
        Assert.Equal(662, firstStar()) // 301 (too low)

    [<Fact>]
    let ``second star`` () =
        Assert.Equal(37312, secondStar())

    [<Theory>]
    [<InlineData(0,3,6,436)>]
    [<InlineData(1,3,2,1)>]
    [<InlineData(2,1,3,10)>]
    [<InlineData(1,2,3,27)>]
    [<InlineData(2,3,1,78)>]
    [<InlineData(3,2,1,438)>]
    [<InlineData(3,1,2,1836)>]
    let ``examples`` (v1: int) (v2: int) (v3: int) (expected: int) =
        let numbers = [v1;v2;v3]

        let lastSpoken = runTo numbers 2020

        Assert.Equal(expected, lastSpoken)
        


