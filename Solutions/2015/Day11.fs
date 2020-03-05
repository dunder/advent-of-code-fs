module AoC.E2015.Day11

open System


let input = "vzbxkghb"

 
let forbidden = Set.ofList ['i';'o';'l']

let increment (c:char) =

    let rec _increment (c:char) =
    
        let n = (c |> int) + 1
        let nc = n |> char
        match nc with
        | x when x > 'z' -> 'a', true
        | x when forbidden.Contains x -> _increment x
        | _ -> nc, false

    _increment c

let next (current:string) =

    let letters = 
        current 
        |> Seq.rev
        |> Seq.fold (fun (overflow, acc) letter -> 
                if overflow then
                    let (nextLetter, overflow) = increment letter
                    (overflow, nextLetter::acc)
                else 
                    (false, letter::acc)
            ) (true, [])
        |> snd 

    String.Join("", letters)

let straight (input:seq<char>) =
    input |> Seq.pairwise |> Seq.forall (fun (a,b) -> (a |> int) + 1 = (b |> int))

let hasStraight input =
    input 
    |> Seq.windowed 3 
    |> Seq.exists straight


let hasNotForbiddenLetter (input:string) =
    input |> Seq.exists (fun c -> forbidden |> (Set.exists (fun s -> s = c))) |> not

let hasAtLeastTwoDoubledLetters (input:string) =
    let doubles = 
        input
        |> Seq.fold (fun acc letter ->
            match acc with
            | (n, previous)::tl when letter = previous && n = 2 -> (1, letter)::tl
            | (n, previous)::tl when letter = previous -> (n+1, previous)::tl
            | _ -> (1, letter)::acc) []
        |> Seq.filter (fun (count, _) -> count = 2)
        |> Seq.length
        
    doubles > 1

let isValidPassword password =
    hasStraight password && 
    hasNotForbiddenLetter password && 
    hasAtLeastTwoDoubledLetters password

let nextPassword pwd = 
    Seq.unfold (fun password -> 
        let nextPassword = next password
        Some (nextPassword, nextPassword)) pwd
    |> Seq.filter isValidPassword
    |> Seq.head

let firstStar () =
    
    nextPassword input

let secondStar () =

    nextPassword (nextPassword input)

module Tests =

    open Xunit

    [<Fact>]
    let ``first star`` () =
        
        Assert.Equal("vzbxxyzz", firstStar())

    [<Fact>]
    let ``second star`` () =

        Assert.Equal("vzcaabcc", secondStar())

    
    [<Theory>]
    [<InlineData('a', 'b', false)>]
    [<InlineData('z', 'a', true)>]
    let ``increment tests`` current expectedNext expectedOverflow =
        let next = increment current
        let expected = (expectedNext, expectedOverflow)
        Assert.Equal(expected, next)


    [<Theory>]
    [<InlineData("abc", "abd")>]
    [<InlineData("abz", "aca")>]
    [<InlineData("azz", "baa")>]
    let ``next tests`` password expected =

        let n = next password

        Assert.Equal(expected, n)

    [<Theory>]
    [<InlineData("abc")>]
    [<InlineData("abcd")>]
    [<InlineData("zbcd")>]
    [<InlineData("zbcde")>]
    [<InlineData("gijkf")>]
    [<InlineData("gijklf")>]
    let ``hasStraight true`` input =
        let actual = hasStraight input

        Assert.True(actual)

    [<Theory>]
    [<InlineData("abd")>]
    [<InlineData("abdc")>]
    [<InlineData("zacd")>]
    [<InlineData("zbbbe")>]
    [<InlineData("gikkf")>]
    [<InlineData("gijjkkf")>]
    let ``hasStraight false`` input =
        let actual = hasStraight input

        Assert.False(actual)

    [<Fact>]
    let ``hasNotForbidden true``() =
        let actual = hasNotForbiddenLetter "abcdefghjkmnpqrstuvwzyx"

        Assert.True(actual)

    [<Theory>]
    [<InlineData("i")>]
    [<InlineData("o")>]
    [<InlineData("l")>]
    [<InlineData("iol")>]
    [<InlineData("gif")>]
    [<InlineData("gof")>]
    [<InlineData("glf")>]
    [<InlineData("lf")>]
    [<InlineData("lf")>]
    [<InlineData("lf")>]
    [<InlineData("fl")>]
    [<InlineData("fl")>]
    [<InlineData("fl")>]
    let ``hasNotForbidden false`` input =
        let actual = hasNotForbiddenLetter input

        Assert.False(actual)

    [<Theory>]
    [<InlineData("aabcc")>]
    let ``hasAtLeastTwoDoubledLetters true`` input =
        let actual = hasAtLeastTwoDoubledLetters input

        Assert.True(actual)

    [<Theory>]
    [<InlineData("aaa")>]
    let ``hasAtLeastTwoDoubledLetters false`` input =
        let actual = hasAtLeastTwoDoubledLetters input

        Assert.False(actual)

    [<Theory>]
    [<InlineData("abcdefgh", "abcdffaa")>]
    [<InlineData("ghijklmn", "ghjaabcc")>]
    let ``nextPassword tests`` password expectedNext =
        let actual = nextPassword password

        Assert.Equal(expectedNext, actual)
        

