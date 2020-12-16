module AoC.E2020.Day16

open AoC
open IO
open System.Text.RegularExpressions

// --- Day 16: Ticket Translation ---

let input = readInputLines "2020" "Day16" |> List.ofSeq

type Ticket = list<int>

type Range = int*int

type Rule = {
    Name: string
    Ranges: Range*Range
}

type RuleSet = list<Rule>

module Ticket = 
    let toTicket (line: string) = line.Split(",") |> Array.map int |> List.ofArray
    let isTicket (line: string) = line |> String.exists ((=) ',')

module Range = 
    let isWithin value (start, stop) = 
        value >= start && value <= stop

module Rule =
    let isWithin value (rule: Rule) =
        rule.Ranges |> fst |> Range.isWithin value
        ||
        rule.Ranges |> snd |> Range.isWithin value

    let isMatching values (rule: Rule) =
        values |> List.forall (fun value -> rule |> isWithin value)

module RuleSet = 

    let anyValidRule value (ruleSet: RuleSet) =
        ruleSet |> List.exists (fun rule -> rule |> Rule.isWithin value)

    let isTicketValid (ticket: Ticket) (ruleSet: RuleSet) =
        ticket |> List.forall (fun value -> ruleSet |> anyValidRule value)

    let invalidValues (ticket: Ticket) (ruleSet: RuleSet) = 
        ticket |> Seq.filter (fun value -> not <| (ruleSet |> anyValidRule value))

    let countMatchingRules (values: list<int>) (ruleSet: RuleSet) =
        ruleSet |> List.filter (Rule.isMatching values) |> List.length

let parseRules lines = 
    let regex = Regex(@"(.+): (\d+)-(\d+) or (\d+)-(\d+)")
    lines
    |> List.filter regex.IsMatch
    |> List.map (fun line ->
        let m = regex.Match(line)
        let name = m.Groups.[1].Value
        let start1 =  m.Groups.[2].Value |> int
        let stop1 =  m.Groups.[3].Value |> int
        let start2 =  m.Groups.[4].Value |> int
        let stop2 =  m.Groups.[5].Value |> int
        { Name = name; Ranges = ((start1, stop1), (start2, stop2))}
    )

let parseMyTicket lines = 
    lines 
    |> List.find Ticket.isTicket 
    |> Ticket.toTicket

let parseNearbyTickets lines = 
    lines 
    |> List.filter Ticket.isTicket 
    |> List.skip 1
    |> List.map Ticket.toTicket

let ticketScanningErrorRate lines = 
    let rules = parseRules lines
    let nearbyTickets = parseNearbyTickets lines

    nearbyTickets 
    |> Seq.collect (fun ticket ->  rules |> RuleSet.invalidValues ticket)
    |> Seq.fold (+) 0

let mapFields (tickets: list<Ticket>) (ruleSet: RuleSet) =

    let values index (tickets: list<Ticket>) =
        tickets |> List.map (fun ticket -> ticket.[index])
    
    let indecesSortedByRuleMatch = 
        [0..tickets.Head.Length-1]
        |> List.map (fun index -> index, ruleSet |> RuleSet.countMatchingRules (values index tickets))
        |> List.sortBy snd
    
    let alreadyMatched = Set.empty
    let fieldMap = Map.empty

    let map, _ = 
        indecesSortedByRuleMatch
        |> List.map fst
        |> List.fold (fun (map, matched) i ->
            let vs = values i tickets
            let matchingRule = 
                ruleSet 
                |> List.find (fun rule -> 
                    let matchesRule = rule |> Rule.isMatching vs
                    let notMatchedAlready =  not <| (matched |> Set.contains rule.Name)
                    matchesRule && notMatchedAlready
                )
            (map |> Map.add matchingRule.Name i, matched |> Set.add matchingRule.Name)
        ) (fieldMap, alreadyMatched)

    map

let firstStar () =
    ticketScanningErrorRate input

let secondStar () = 

    let rules = parseRules input
    let myTicket = parseMyTicket input
    let nearbyTickets = parseNearbyTickets input

    let validTickets = 
        nearbyTickets
        |> List.filter (fun ticket -> rules |> RuleSet.isTicketValid ticket)

    let fieldSet = mapFields validTickets rules

    rules
    |> List.filter (fun rule -> rule.Name.StartsWith("departure"))
    |> List.map (fun rule -> myTicket.[fieldSet.[rule.Name]] |> int64)
    |> List.fold (*) 1L
    

module Tests =

    open Xunit

    [<Fact>]
    let ``first star`` () =
        Assert.Equal(26053, firstStar())

    [<Fact>]
    let ``second star`` () =
        Assert.Equal(1515506256421L, secondStar())

    [<Fact>]
    let ``first star example`` () =
        let input = [
            "class: 1-3 or 5-7"
            "row: 6-11 or 33-44"
            "seat: 13-40 or 45-50"
            ""
            "your ticket:"
            "7,1,14"
            ""
            "nearby tickets:"
            "7,3,47"
            "40,4,50"
            "55,2,20"
            "38,6,12"
        ]

        let rate = ticketScanningErrorRate input

        Assert.Equal(71, rate)

    [<Fact>]
    let ``second star example`` () =
        let input = [
            "class: 0-1 or 4-19"
            "row: 0-5 or 8-19"
            "seat: 0-13 or 16-19"
            ""
            "your ticket:"
            "11,12,13"
            ""
            "nearby tickets:"
            "3,9,18"
            "15,1,5"
            "5,14,9"
        ]

        let rules = parseRules input
        let myTicket = parseMyTicket input
        let nearbyTickets = parseNearbyTickets input

        let validTickets = 
            nearbyTickets
            |> List.filter (fun ticket -> rules |> RuleSet.isTicketValid ticket)

        let fieldSet = mapFields validTickets rules

        let class' = myTicket.[fieldSet.["class"]]
        let row = myTicket.[fieldSet.["row"]]
        let seat = myTicket.[fieldSet.["seat"]]

        Assert.Equal(class', 12)
        Assert.Equal(row, 11)
        Assert.Equal(seat, 13)
