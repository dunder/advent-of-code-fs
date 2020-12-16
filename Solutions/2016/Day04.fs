module AoC.E2016.Day04

open AoC
open IO
open System.Text.RegularExpressions

// --- Day 4: Security Through Obscurity ---

let input = readInputLines "2016" "Day04" |> List.ofSeq

type RoomIdentifier = { Name: string; Id: int; Checksum: string }


let toRoomIdentifier line = 
    let regex = Regex("([a-z\-]+)(\d+)\[([a-z]{5})\]")
    let m = regex.Match(line)
    let name = m.Groups.[1].Value
    let id = m.Groups.[2].Value |> int
    let checksum = m.Groups.[3].Value

    { Name = name; Id = id; Checksum = checksum }

let parse lines = lines |> List.map toRoomIdentifier

let countLetters room = 
    room.Name 
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

    checksum = roomIdentifier.Checksum

let alphabetLength = ('z' |> int) - ('a' |> int) + 1
let a = 'a' |> int

let shift times c = 
    match c with
    | '-' -> ' '
    | c -> ((c |> int) - a + times) % alphabetLength + a |> char

let decrypt (room: RoomIdentifier) = room.Name |> String.map (shift room.Id) |> System.String.Concat

let firstStar () =
    parse input
    |> List.filter isValidChecksum
    |> List.sumBy (fun x -> x.Id)

let secondStar () = 
    let northPoleObjects =
        parse input
        |> List.filter isValidChecksum
        |> List.map (fun room -> decrypt room, room)
        |> List.find (fun (decrypted, room) -> decrypted.Contains("northpole"))
        
    (northPoleObjects |> snd).Id


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
    let ``valid checksum`` (identifier: string) (valid: bool) =
        
        let isValid = isValidChecksum (toRoomIdentifier identifier)
        Assert.Equal(valid, isValid)

    [<Theory>]
    [<InlineData('a', 1, 'b')>]
    [<InlineData('z', 1, 'a')>]
    let ``shift`` (c: char) (times: int) (expected: char) =
        
        let shifted = shift times c

        Assert.Equal(expected, shifted)

    [<Fact>]
    let ``decrypt`` () =
        
        let room = toRoomIdentifier "qzmt-zixmtkozy-ivhz-343[zimth]"

        let decrypted = decrypt room

        Assert.Equal("very encrypted name ", decrypted)