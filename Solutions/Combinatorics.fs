module Combinatorics


// returns a list of lists of all the combinations of n of the list of l, see the examples below
let rec combinations n l = 
    match n, l with
    | 0, _ -> [[]]
    | _, [] -> []
    | k, (x::xs) -> List.map ((@) [x]) (combinations (k-1) xs) @ combinations k xs

// returns a sequence of all the permutations of the provided list, from https://stackoverflow.com/a/2184129/3047206
let rec permutations = function
       | [] -> seq [List.empty]
       | x :: xs -> Seq.collect (insertions x) (permutations xs)
   and insertions x = function
       | [] -> [[x]]
       | (y :: ys) as xs -> (x::xs)::(List.map (fun x -> y::x) (insertions x ys))

module Tests =

    open Xunit

    [<Fact>]
    let ``combinations example 1`` () =
        let actual = combinations 2 [1..3]

        Assert.Equal(3, List.length actual)

        Assert.Contains(actual, fun combination -> combination |> Seq.length = 2 && combination |> Seq.contains 1 && combination |> Seq.contains 2)
        Assert.Contains(actual, fun combination -> combination |> Seq.length = 2 && combination |> Seq.contains 2 && combination |> Seq.contains 3)
        Assert.Contains(actual, fun combination -> combination |> Seq.length = 2 && combination |> Seq.contains 1 && combination |> Seq.contains 3)

    [<Fact>]
    let ``combinations example 2`` () =
        let actual = combinations 3 [1..3]
        
        Assert.Equal(1, List.length actual)
        Assert.Contains(1, actual.[0])
        Assert.Contains(2, actual.[0])
        Assert.Contains(3, actual.[0])
        
    [<Fact>]
    let ``permutations example`` () =
        let actual = permutations [1..3]
            
        Assert.Equal(6, Seq.length actual)
        Assert.Contains([1;2;3], actual)
        Assert.Contains([1;3;2], actual)
        Assert.Contains([2;3;1], actual)
        Assert.Contains([2;1;3], actual)
        Assert.Contains([3;1;2], actual)
        Assert.Contains([3;2;1], actual)
