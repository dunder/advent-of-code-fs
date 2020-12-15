module AoC.E2016.Day04

open AoC
open IO
open System.Text.RegularExpressions

// --- Day 4: Security Through Obscurity ---

let input = readInputLines "2016" "Day04" |> List.ofSeq

type RoomIdentifier = { name: string; id: int; checksum: string }


let toRoomIdentifier line = 
    let regex = Regex("([a-z\-]+)(\d+)\[([a-z]{5})\]")
    let m = regex.Match(line)
    let name = m.Groups.[1].Value
    let id = m.Groups.[2].Value |> int
    let checksum = m.Groups.[3].Value

    { name = name; id = id; checksum = checksum }

let parse lines = lines |> List.map toRoomIdentifier


let countLetters room = 
    room.name 
    |> Seq.filter System.Char.IsLetter 
    |> Seq.groupBy (id)
    |> Seq.map (fun (key, values) -> (key, values |> Seq.length))

let letterCountSort (c1, count1) (c2, count2) = 
    if count1 <> count2 then
        count2 - count1
    else
        (c1 |> int) - (c2 |> int)
    

let isValidChecksum roomIdentifier =
    
    let checksum = 
        countLetters roomIdentifier
        |> Seq.sortWith letterCountSort 
        |> Seq.take 5 
        |> Seq.map fst
        |> System.String.Concat

    checksum = roomIdentifier.checksum

let firstStar () =
    let x = parse input
    let y = countLetters { name = "aaaaa-bbb-z-y-x-"; id = 123; checksum = "abxyz" }

    0

let secondStar () = 
    0


module Tests =

    open Xunit

    [<Fact>]
    let ``first star`` () =

        Assert.Equal(137896, firstStar())

    [<Fact>]
    let ``second star`` () =

        Assert.Equal(501, secondStar())

    [<Theory>]
    [<InlineData("aaaaa-bbb-z-y-x-123[abxyz]", true)>]
    [<InlineData("a-b-c-d-e-f-g-h-987[abcde]", true)>]
    [<InlineData("not-a-real-room-404[oarel]", true)>]
    [<InlineData("totally-real-room-200[decoy]", false)>]
    let ``isValidChecksum`` (identifier: string) (valid: bool) =
        
        let isValid = isValidChecksum (toRoomIdentifier identifier)
        Assert.Equal(valid, isValid)