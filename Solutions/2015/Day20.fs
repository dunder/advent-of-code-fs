module AoC.E2015.Day20

open AoC
open IO

// --- Day 20: Infinite Elves and Infinite Houses ---

let input = 36000000

// well had to get help this time https://www.reddit.com/r/adventofcode/comments/3xjpp2/day_20_solutions/

let firstStar () =
    
    let houses = Array.zeroCreate input

    for elf in 1..input/10 do
        for house in elf..elf..input/10 do  
            houses.[house] <- houses.[house] + elf*10

    houses |> Seq.findIndex (fun x -> x >= input)

let secondStar () = 
    
    let houses = Array.zeroCreate input

    for elf in 1..input/10 do
        let potentialHouses = {elf..elf..input/10}
        let housesToVisit =
            if Seq.length potentialHouses > 50 then
                potentialHouses |> Seq.take 50
            else    
                potentialHouses
        for house in housesToVisit do  
            houses.[house] <- houses.[house] + elf*11

    houses |> Seq.findIndex (fun x -> x >= input)


module Tests =

    open Xunit

    [<Fact>]
    let ``first star`` () =
        Assert.Equal(831600, firstStar())

    [<Fact>]
    let ``second star`` () =
        Assert.Equal(884520, secondStar())
        


