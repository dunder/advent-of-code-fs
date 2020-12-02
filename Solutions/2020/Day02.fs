module AoC.E2020.Day02

open AoC
open IO
open Sequences
open System.Text.RegularExpressions


// --- Day 2: Password Philosophy ---

let input = readInputLines "2020" "Day02" |> List.ofSeq

// active pattern, captured groups are returned in a list option
let (|Regex|_|) pattern input = 
    let m = Regex.Match(input, pattern)

    if m.Success
    then Some(List.tail [for g in m.Groups -> g.Value])
    else None

type PasswordPolicyCheck = { Low: int; High: int; Letter: char; Password: string }

let parse lines =
    
    lines
    |> Seq.map (fun line -> 
        match line with
        | Regex "(\d*)-(\d+) ([a-z]): ([a-z]+)" [ low; high; letter; password ] ->
            let low = if low = "" then 0 else low |> int
            { Low = low ; High = high |> int; Letter = letter |> char; Password = password}
        | _ -> failwithf "Unexpected password policy: %s" line  
    )

let valid (policyCheck:PasswordPolicyCheck) =
    let letterCount = policyCheck.Password |> count policyCheck.Letter
    letterCount >= policyCheck.Low && letterCount <= policyCheck.High

let valid2 (policyCheck:PasswordPolicyCheck) =

    let password = policyCheck.Password

    let within index =
        index >= 0 && index < String.length password

    let letter = policyCheck.Letter
    let lowIndex = policyCheck.Low-1
    let highIndex = policyCheck.High-1

    let hasLow = (within lowIndex) && password.[lowIndex] = letter
    let hasHigh = (within highIndex) && password.[highIndex] = letter

    hasLow <> hasHigh

let firstStar () =
    let passwordPolicyChecks = parse input
    passwordPolicyChecks
    |> Seq.filter valid
    |> Seq.length
        

let secondStar () =
    let passwordPolicyChecks = parse input
    passwordPolicyChecks
    |> Seq.filter valid2
    |> Seq.length


module Tests =

    open Xunit

    [<Fact>]
    let ``first star`` () =
        Assert.Equal(414, firstStar())

    [<Fact>]
    let ``second star`` () =
        Assert.Equal(413, secondStar())
