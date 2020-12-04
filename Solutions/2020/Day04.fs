module AoC.E2020.Day04

open AoC
open IO
open System.Text.RegularExpressions

// --- Day 4: Passport Processing ---

let input = readInputLines "2020" "Day04" |> List.ofSeq

let requiredFields = ["byr";"iyr";"eyr";"hgt";"hcl";"ecl";"pid"]

// active pattern, captured groups are returned in a list option
let (|Regex|_|) pattern input = 
    let m = Regex.Match(input, pattern)

    if m.Success
    then Some(List.tail [for g in m.Groups -> g.Value])
    else None

let (|Int|_|) (str:string) =
    match System.Int32.TryParse str with
    | true,int -> Some int
    | _ -> None

let toPassportFieldSet (passportData:string) =
    passportData.Split " " 
    |> Seq.map (fun s -> s.[0..s.IndexOf ":"-1])
    |> Set.ofSeq
 
let toPassportFieldMap (passportData:string) =
    passportData.Split " " 
    |> Seq.map (fun data ->
        let fieldValue = data.Split ":"
        (fieldValue.[0], fieldValue.[1])
    )
    |> Map.ofSeq
 
let valid passportFieldSet =
    requiredFields |> Seq.forall (fun f -> passportFieldSet |> Set.contains f)

let validBirthyear value =
    match value with
    | Int x -> 1920 <= x && x <= 2002
    | _ -> false

let validIssueYear value = 
    match value with
    | Int x -> 2010 <= x && x <= 2020
    | _ -> false

let validExpirationYear value =
    match value with
    | Int x -> 2020 <= x && x <= 2030
    | _ -> false

let validHeight value =
    match value with
    | Regex "(\d+)(in|cm)" [height;unit] ->
        let height = height |> int
        match unit with
        | "cm" -> 150 <= height && height <= 193
        | "in" -> 59 <= height && height <= 76
    | _ -> false

let validHairColor value = 
    Regex.IsMatch(value, "#[0-9a-f]{6}")

let validEyeColor value =
    Regex.IsMatch(value, "(amb|blu|brn|gry|grn|hzl|oth)")

let validPassportId (value:string) =
    value |> Seq.length = 9
    &&
    fst (System.Int32.TryParse value)

let validCountryId _ =
    true

let valid2 passportFieldMap = 
    requiredFields |> Seq.forall (fun f -> passportFieldMap |> Map.containsKey f)
    &&
    passportFieldMap
    |> Seq.forall (fun kvp -> 
        match kvp.Key with
        | "byr" -> validBirthyear kvp.Value
        | "iyr" -> validIssueYear kvp.Value
        | "eyr" -> validExpirationYear kvp.Value
        | "hgt" -> validHeight kvp.Value
        | "hcl" -> validHairColor kvp.Value
        | "ecl" -> validEyeColor kvp.Value
        | "pid" -> validPassportId kvp.Value
        | "cid" -> validCountryId kvp.Value
        | _ -> failwithf "Unmatch field: %s %s" kvp.Key kvp.Value
    )

let parse lines =
    lines
    |> Seq.unfold (fun lines -> 
        if lines |> Seq.length = 0 then
            None
        else
            let nextPassportLines = lines |> Seq.takeWhile (fun line -> line <> "")
            if lines |> Seq.length = (nextPassportLines |> Seq.length) then
                Some (nextPassportLines, Seq.empty)
            else
                let theRest = lines |> Seq.skipWhile (fun line -> line <> "") |> Seq.tail
                Some (nextPassportLines, theRest)
    )
    |> Seq.map (fun passportLines -> passportLines |> String.concat " ")
    

let firstStar () =
    parse input |> List.ofSeq
    |> List.map toPassportFieldSet
    |> List.filter valid
    |> List.length

let secondStar () = 
    parse input |> List.ofSeq
    |> List.map toPassportFieldMap
    |> List.filter valid2
    |> List.length

module Tests =

    open Xunit

    [<Fact>]
    let ``first star`` () =
        Assert.Equal(260, firstStar())

    [<Fact>]
    let ``second star`` () =
        Assert.Equal(153, secondStar())
        
