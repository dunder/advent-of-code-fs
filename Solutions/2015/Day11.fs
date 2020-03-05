module AoC.E2015.Day11

open System


let input = "vzbxkghb"

type Letter = private Letter of char

module Letter  =
    let create char = 
        if char < 'a' || char > 'z' then
            failwithf "A letter must be lowercase between 'a' and 'z' but was '%c'" char
        else
            Letter char

    let value (Letter char) =
        char
 
let password input =
    input |> Seq.map Letter.create


let increment (current:Letter) =
    
    let n = (current |> Letter.value |> int) + 1
    let z = 'z' |> int
    if n > z then
        Letter.create 'a', true
    else 
        n |> char |> Letter.create, false

let next current =
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
        |> Seq.map Letter.value

    String.Join("", letters)

let straight (input:seq<char>) =
    input |> Seq.pairwise |> Seq.forall (fun (a,b) -> (a |> int) + 1 = (b |> int))

let hasStraight input =
    input 
    |> Seq.windowed 3 
    |> Seq.exists straight

let forbidden = Set.ofList ['i';'o';'l']

let hasNotForbiddenLetter (input:string) =
    input |> Seq.exists (fun c -> forbidden |> (Set.exists (fun s -> s = c))) |> not
   

let firstStar () =

    ""

let secondStar () =

    ""

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
        let next = increment <| Letter.create current
        let expected = (Letter.create expectedNext, expectedOverflow)
        Assert.Equal(expected, next)


    [<Theory>]
    [<InlineData("abc", "abd")>]
    [<InlineData("abz", "aca")>]
    [<InlineData("azz", "baa")>]
    let ``next tests`` input expected =
        let actual = password input |> List.ofSeq

        let n = next actual

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
        

