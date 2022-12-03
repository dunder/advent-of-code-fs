module AoC.E2020.Day04

open AoC
open IO
open ActivePatterns
open System.Text.RegularExpressions

// --- Day 4: Passport Processing ---

let input = readInputLines "2020" "Day04" |> List.ofSeq

let requiredFields = ["byr";"iyr";"eyr";"hgt";"hcl";"ecl";"pid"]

let parse lines =

    lines
    |> Seq.mapFold (fun acc line -> 
       if line = "" then
            ((-1,line), acc + 1)
       else
            ((acc,line), acc)
    ) 0
    |> fst
    |> Seq.filter (fun (i, _) -> i <> -1)
    |> Seq.groupBy (fun (i, _) -> i)
    |> Seq.map (fun x -> snd x |> Seq.map snd |> String.concat " ")

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
        | _ -> failwithf "Unrecognized unit: %s" unit
    | _ -> false

let validHairColor (value: string) = 
    Regex.IsMatch(value, "#[0-9a-f]{6}")

let validEyeColor (value: string) =
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
        
