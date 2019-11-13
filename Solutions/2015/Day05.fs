module AoC.E2015.Day05

open AoC
open IO
open System.Text.RegularExpressions

// --- Day 5: Doesn't He Have Intern-Elves For This? ---

let input = readInputLines "2015" "Day05"


let isVowel (c:char) =
    "aeiou".Contains(c)

let atLeastThreeVowels s = s |> Seq.filter isVowel |> Seq.length > 2

let letterRepeated s = s |> Seq.pairwise |> Seq.filter (fun (x, y) -> x = y) |> Seq.length > 0

let badCombinations s = 
    let bad = Set.ofList ["ab";"cd";"pq";"xy"]
    s |> Seq.pairwise |> Seq.exists (fun (x, y) -> bad.Contains(sprintf "%c%c" x y))

let isNice s =
    s |> atLeastThreeVowels && s |> letterRepeated && not (s|> badCombinations)

let rec twoConsecutiveLettersTwice s =
    if (String.length s < 3) then
        false
    else
        let head = s.[0..1]
        let tail = s.[2..]

        if (String.length tail = 0) then
            false
        else
            if (tail.Contains(head)) then
                true
            else 
                twoConsecutiveLettersTwice s.[1..]

let letterRepeatedWithAnyOtherLetterBetween s = 
    let expr = Regex @"([a-z])[a-z]\1"
    expr.IsMatch s
    

let isNice2 s = 
    twoConsecutiveLettersTwice s && letterRepeatedWithAnyOtherLetterBetween s

let firstStar () =
    input |> Seq.filter isNice |> Seq.length

let secondStar () = 
    input |> Seq.filter isNice2 |> Seq.length


module Tests =

    open Xunit

    [<Theory>]
    [<InlineData("aei")>]
    [<InlineData("xazegov")>]
    [<InlineData("aeiouaeiouaeiou")>]
    let ``has three vowels`` (input) =
        Assert.True(atLeastThreeVowels input)

    [<Theory>]
    [<InlineData("abcdde")>]
    [<InlineData("aabbccdd")>]
    let ``has at least one letter sequentially repeated`` (input) =
        Assert.True(letterRepeated input)

    [<Theory>]
    [<InlineData("ugknbfddgicrmopn")>]
    [<InlineData("aaa")>]
    let ``is nice`` (input) =
        Assert.True(isNice input)

    [<Theory>]
    [<InlineData("jchzalrnumimnmhp")>]
    [<InlineData("haegwjzuvuyypxyu")>]
    [<InlineData("dvszwmarrgswjxmb")>]
    let ``is naughty`` (input) = 
        Assert.False(isNice input)

    [<Theory>]
    [<InlineData("xyxy")>]
    [<InlineData("aabcdefgaa")>]
    let ``two consecutive letters repeated twice`` (input) =
        Assert.True(twoConsecutiveLettersTwice input)

    [<Fact>]
    let ``two consecutive letters with one between when overlap`` () =
        Assert.False(twoConsecutiveLettersTwice "aaa")

    [<Fact>]
    let ``two consecutive letters with one between`` () =
        let actual = letterRepeatedWithAnyOtherLetterBetween "astompop"
        Assert.True(actual)

    [<Theory>]
    [<InlineData("qjhvhtzxzqqjkmpb")>]
    [<InlineData("xxyxx")>]
    let ``is nice 2`` (input) =
        Assert.True(isNice2 input)

    [<Theory>]
    [<InlineData("uurcxstgmygtbstg")>]
    [<InlineData("ieodomkazucvgmuy")>]
    let ``is naughty 2`` (input) = 
        Assert.False(isNice2 input)

    [<Fact>]
    let ``first star`` () =
        Assert.Equal(238, firstStar());

    [<Fact>]
    let ``second star`` () =
        Assert.Equal(69, secondStar())
