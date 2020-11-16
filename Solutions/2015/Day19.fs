module AoC.E2015.Day19

open System.Text.RegularExpressions;

open AoC
open IO

// --- Day 19: Medicine for Rudolph ---

let input = readInputLines "2015" "Day19"

type Replacement = { Match:string; Generates:string}

let (|Regex|_|) pattern input = 
    let m = Regex.Match(input, pattern)
    
    if m.Success 
    then Some(List.tail [for g in m.Groups -> g.Value])
    else None

let parseReplacements (lines:seq<string>) = 
    lines
    |> Seq.filter (fun line -> line.Contains("=>"))
    |> Seq.map (fun line -> 
        match line with
        | Regex "(.*) => (.*)" [m;g] -> { Match = m; Generates = g }
        | _ -> failwithf "Unrecognized input %s" line
    )

let parseMolecule lines = lines |> Seq.last
    

let firstStar () =
    let replacements = parseReplacements input
    let molecule = parseMolecule input
    
    replacements
    |> Seq.map (fun r -> 
        Regex.Matches(molecule, r.Match)
        |> Seq.map (fun m ->
            let regEx = new Regex(r.Match)
            regEx.Replace(molecule, r.Generates, 1, m.Index)
        )
    )
    |> Seq.collect (fun x -> x)
    |> Seq.distinct
    |> Seq.length
    
let secondStar () = 
    0


module Tests =

    open Xunit

    [<Fact>]
    let ``first star`` () =

        Assert.Equal(509, firstStar())

    [<Fact>]
    let ``second star`` () =

        Assert.Equal(-1, secondStar())
