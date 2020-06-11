module AoC.E2015.Day16

open System.Text.RegularExpressions

open AoC
open IO

// --- Day 16: Aunt Sue ---

let input = readInputLines "2015" "Day16"

let (|Regex|_|) pattern input = 
    let m = Regex.Match(input, pattern)

    if m.Success
    then Some(List.tail [for g in m.Groups -> g.Value])
    else None

type Sue = { Id:int; Attributes:(string*int) list }
 
let parse input = 
    input
    |> Seq.map (fun line -> 
        match line with 
        | Regex "Sue (\d+): ([a-z]+): (\d+), ([a-z]+): (\d+), ([a-z]+): (\d+)" 
            [id; attribute1; value1; attribute2; value2; attribute3; value3] -> { 
                Id = id |> int
                Attributes = [
                    (attribute1, value1 |> int)
                    (attribute2, value2 |> int)
                    (attribute3, value3 |> int)
                ]
            }
        | x -> failwithf "Cannot parse: %s" x
    )

let sueAttributes = [
    ("children", 3)
    ("cats", 7)
    ("samoyeds", 2)
    ("pomeranians", 3)
    ("akitas", 0)
    ("vizslas", 0)
    ("goldfish", 5)
    ("trees", 3)
    ("cars", 2)
    ("perfumes", 1)
]

let isSue sue =
    let sueAttributeSet = Set.ofList sueAttributes
    sue.Attributes |> Seq.forall (fun a -> Set.contains a sueAttributeSet)

let isReallySue sue =

    sue.Attributes |> Seq.forall (fun (attr, n) -> 
        let matchedAttribute = sueAttributes |> Seq.filter (fun (attr', n') -> attr' = attr) |> Seq.exactlyOne
        match matchedAttribute with
        | ("cats", x) | ("trees", x) -> n > x
        | ("pomeranians", x) | ("goldfish", x) -> n < x
        | (_, x) -> n = x
    )

let firstStar () =
    let sues = parse input
    let sue = 
        sues
        |> Seq.filter isSue
        |> Seq.exactlyOne
    sue.Id
        
let secondStar () = 
    let sues = parse input
    let sue = 
        sues
        |> Seq.filter isReallySue
        |> Seq.exactlyOne
    sue.Id
    

module Tests =

    open Xunit

    [<Fact>]
    let ``first star`` () =

        Assert.Equal(40, firstStar())

    [<Fact>]
    let ``second star`` () =

        Assert.Equal(241, secondStar())
    
    [<Fact>]
    let ``is really Sue?`` () =

        let sue = { Id = 1; Attributes = [("cats", 8);("trees", 4);("goldfish", 4)]}
        let isReallySue = isReallySue sue
        Assert.True(isReallySue)