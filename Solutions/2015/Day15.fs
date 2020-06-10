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

let score ingredients amounts =
    let ingredientAmounts = amounts |> List.zip ingredients
    [
        ingredientAmounts 
            |> Seq.sumBy (fun (ingredient, amount) -> ingredient.Capacity*amount)
        ingredientAmounts 
            |> Seq.sumBy (fun (ingredient, amount) -> ingredient.Durability*amount)
        ingredientAmounts 
            |> Seq.sumBy (fun (ingredient, amount) -> ingredient.Flavor*amount)
        ingredientAmounts 
            |> Seq.sumBy (fun (ingredient, amount) -> ingredient.Texture*amount)
    ] 
    |> Seq.map (fun x -> max 0 x)
    |> Seq.reduce (fun x y -> x * y)
    

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

    let rec loop ingredients currentAmount accPermutations = 
        
        seq {
            match ingredients with
            | [] -> yield accPermutations
            | [_] -> yield (totalAmount-currentAmount)::accPermutations
            | _::xs -> 
                for n in 0..totalAmount - currentAmount do
                    yield! loop xs (currentAmount + n) (n::accPermutations)
        }

    loop ingredients 0 []

let firstStar () =
    let ingredients = List.ofSeq <| parse input
    let permutations = permutations ingredients 100
    permutations
        |> Seq.map (fun p -> score ingredients p)
        |> Seq.max
        
let secondStar () = 
    0


module Tests =

    open Xunit

    [<Fact>]
    let ``first star`` () =

        Assert.Equal(13882464, firstStar())

    [<Fact>]
    let ``second star`` () =

        Assert.Equal(-1, secondStar())
        
    [<Fact>]
    let ``first star example`` () =
        
        let ingredients = [
            { Name = "Butterscotch"; Capacity = -1; Durability = -2; Flavor = 6; Texture = 3; Calories = 8 }
            { Name = "Cinnamon"; Capacity = 2; Durability = 3; Flavor = -2; Texture = -1; Calories = 3 }
        ]
        let amounts = [44;56]
        let score = score ingredients amounts
        Assert.Equal(62842880, score)