module AoC.E2015.Day15

open System.Text.RegularExpressions

open AoC
open IO

// --- Day 15: Science for Hungry People ---

let input = readInputLines "2015" "Day15"

let (|Regex|_|) pattern input = 
    let m = Regex.Match(input, pattern)

    if m.Success
    then Some(List.tail [for g in m.Groups -> g.Value])
    else None

type Ingredient = { Name: string; Capacity: int; Durability: int; Flavor: int; Texture: int; Calories: int}

let parse input = 
    input
    |> Seq.map (fun line -> 
        match line with 
        | Regex "^(.*): capacity (-?\d+), durability (-?\d+), flavor (-?\d+), texture (-?\d+), calories (-?\d+)" 
            [name; capacity; durability; flavor; texture; calories] -> { 
                Name = name 
                Capacity = capacity |> int
                Durability = durability |> int
                Flavor = flavor |> int
                Texture = texture |> int
                Calories = calories |> int
            }
        | x -> failwithf "Cannot parse: %s" x
    )

let permutations ingredients totalAmount = 

    let rec loop ingredients (currentAmount, acc) = 
        
        seq {
            match ingredients with
            | [ingredient] -> yield (ingredient, totalAmount-currentAmount)::acc
            | ingredient::ingredients -> 
                for n in 0..totalAmount - currentAmount do
                    yield! loop ingredients (currentAmount + n, (ingredient, n)::acc)
            | [] -> yield acc
        }

    loop ingredients (0, [])

let firstStar () =
    let result = permutations ['A';'B'] 100
    0

let secondStar () = 
    0


module Tests =

    open Xunit

    [<Fact>]
    let ``first star`` () =

        Assert.Equal(-1, firstStar())

    [<Fact>]
    let ``second star`` () =

        Assert.Equal(-1, secondStar())