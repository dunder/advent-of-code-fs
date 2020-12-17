module AoC.E2016.Day05

open AoC
open Cryptography

// --- Day 5: How About a Nice Game of Chess? ---

let input = "reyedfim"

let hash index (input: string) =
    md5 (input + (index |> string))

let generate (startIndex: int) (input: string) =
    Seq.unfold (fun (i, (p: string)) -> 
        if p.StartsWith("00000") then
            None
        else
            let next = hash i input
            Some ((i, next), (i + 1, next))
    ) (startIndex, "")
    |> Seq.last

let toLower (s: string) = s |> String.map System.Char.ToLower

let password (input: string) =
    Seq.unfold (fun (i, (pw: string)) ->
        if pw.Length = 8 then
            None
        else
            let idx, c = generate i input
            let current = pw + (c.[5] |> string)
            Some ((i, current), (idx + 1, current))
                
    ) (0, "")
    |> Seq.last
    |> snd
    |> toLower

let password2 (input: string) =
    Seq.unfold (fun (i, pw: list<int*char>) ->
        if pw.Length = 8 then
            None
        else
            let idx, c = generate i input

            let currentCharIdx = c.[5]
            let currentChar = c.[6]

            let next = 
                if System.Char.IsDigit(currentCharIdx) then
                    let num = currentCharIdx |> string |> int
                    let notFoundYet = not <| (pw |> Seq.exists (fun (i, _) -> i = num))
                    if num < 8 && notFoundYet then
                        (num, currentChar)::pw
                    else
                        pw
                else
                    pw

            Some ((i, next), (idx + 1, next))
                
    ) (0, [])
    |> Seq.last
    |> snd
    |> List.sortBy fst
    |> Seq.map snd
    |> System.String.Concat
    |> toLower

let firstStar () =
    password input

let secondStar () = 
    password2 input


module Tests =

    open Xunit

    [<Fact>]
    let ``first star`` () =

        Assert.Equal("f97c354d", firstStar())

    [<Fact>]
    let ``second star`` () =

        Assert.Equal("863dde27", secondStar())

    [<Fact>]
    let ``first star example`` () =

        let generatedPassword = password "abc"

        Assert.Equal("18f47a30", generatedPassword)

    [<Fact>]
    let ``second star example`` () =

        let generatedPassword = password2 "abc"

        Assert.Equal("05ace8e3", generatedPassword)