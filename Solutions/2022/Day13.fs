module AoC.E2022.Day13

// --- Day 13: Distress Signal ---

open AoC
open IO


let input = readInputLines "2022" "Day13" |> List.ofSeq


type Token =
    | TokenList of list<Token>
    | Number of int

let read (text: string) =
        
    let toNumber chars = Number(chars |> List.rev |> List.toArray |> System.String |> int)    

    let rec readLoop (text: string) (acc: list<list<Token>>) (num: list<char>) =
        if text = "" then
            acc
        else
            match text[0] with
            | '[' -> 
                match acc with 
                | [] -> readLoop (text.Substring(1)) ([[]]) (num)
                | x::xs -> readLoop (text.Substring(1)) ([]::x::xs) (num)
            | ',' -> 
                match acc with
                | [] -> failwith "Parse error at ',': No open list"
                | x::xs -> 
                    if List.isEmpty num then
                        readLoop (text.Substring(1)) acc []
                    else
                        readLoop (text.Substring(1)) (((toNumber num)::x)::xs) []
            | ']' ->
                match acc with
                | [] -> failwith "Parse error at ']': No open list"
                | x::xs -> 
                    let withNumber = 
                        if List.isEmpty num then
                            x
                        else 
                            (toNumber num)::x

                    let closed = TokenList(withNumber |> List.rev)
                    
                    if text.Length = 1 then
                        [[closed]]
                    else
                        match xs with
                        | [] -> failwith "Parse error at ']': No list to add to"
                        | x::xs -> readLoop (text.Substring(1)) ((closed::x)::xs) []
            | n -> 
                match acc with 
                | [] -> failwithf "Parse error at %c" n
                | x::xs -> readLoop (text.Substring(1)) acc (n::num)

    (readLoop text [] []) |> List.exactlyOne |> List.exactlyOne

let parse (lines: string list) = 
    lines
    |> List.filter (System.String.IsNullOrEmpty >> not)
    |> List.map read
    |> List.chunkBySize 2
    |> List.map (fun listOf2 -> listOf2[0], listOf2[1])

let next (tokenList: Token) =
    match tokenList with 
    | TokenList x -> 
        match x with
        | head::tail -> head, tail
        | [] -> failwith "Cannot TokenList is empty"
    | Number n -> failwith "Cannot use next when Token is a Number"

let nextPair (pair: Token*Token) =
    pair |> fst |> next, pair |> snd |> next

let rec tokenCompare ((left, right): Token*Token) =    
    match left, right with
    | TokenList leftList, TokenList rightList ->
        match leftList, rightList with
        | [], [] -> 0
        | x::xs, [] -> 1
        | [], x::xs -> -1
        | x1::xs1, x2::xs2 -> 
            let result = tokenCompare (x1, x2) 
            if result = 0 then
                tokenCompare (TokenList(xs1), TokenList(xs2))
            else 
                result
    | Number n1, Number n2 when n1 <> n2 -> compare n1 n2 
    | Number n1, Number n2 -> 0
    | TokenList list1, Number n2 -> tokenCompare (left, TokenList[right])
    | Number n1, TokenList list2 -> tokenCompare (TokenList([left]), right)

let sumOfIndecesOfCorrectPackages lines = 
    lines
    |> parse
    |> List.map tokenCompare
    |> List.indexed
    |> List.filter (fun (i, compare) -> compare = -1)
    |> List.map fst
    |> List.sumBy (fun i -> i + 1)


let firstStar () = input |> sumOfIndecesOfCorrectPackages


let parseToList (lines: string list) = 
    lines
    |> List.filter (System.String.IsNullOrEmpty >> not)
    |> List.map read    

let decoderKey lines =
    let decoderPackage1 = "[[2]]" |> read
    let decoderPackage2 = "[[6]]" |> read
    let sorted = 
        [
            yield! lines |> parseToList
            yield decoderPackage1
            yield decoderPackage2
        ]
        |> List.sortWith (fun x y -> tokenCompare (x,y))
    let index1 = sorted |> List.findIndex ((=) decoderPackage1)
    let index2 = sorted |> List.findIndex ((=) decoderPackage2)
    (index1 + 1) * (index2 + 1)
    

let secondStar () = decoderKey input