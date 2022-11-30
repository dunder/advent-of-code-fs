// --- Day 4: Security Through Obscurity ---

open System.IO

System.Environment.CurrentDirectory <- __SOURCE_DIRECTORY__

let input = File.ReadLines(@".\Input\Day04.txt") |> List.ofSeq

type Room = { EncryptedName : string; Id : int; Checksum: string; }
type DecryptedRoom = { DecryptedName : string; Id : int }


let parse (lines: string list) =

    let parseLine (line:string) =
        let breakIndex = line.LastIndexOf("-")
        {
            EncryptedName = line.Substring(0, breakIndex)
            Id = line.Substring(breakIndex + 1,3) |> int
            Checksum = line.Substring(breakIndex + 5, 5)
        }

    lines |> List.map parseLine

module Room =

    let checksum room =
        let sortBySizeThenLetter (c1, group1) (c2, group2) =    
            // descending
            let groupCompare = compare (Seq.length group2) (Seq.length group1)
            if groupCompare <> 0 then
                groupCompare
            else
                // ascending
                let keyCompare = compare c1 c2
                keyCompare
        room.EncryptedName.Replace("-", "")
        |> Seq.groupBy id
        |> Seq.toArray
        |> Array.sortWith sortBySizeThenLetter
        |> Array.take 5
        |> Array.map fst 
        |> System.String

    let decrypt room =

        let shift (c:char) id = 
            let aInt = 'a' |> int
            let position = ((c |> int) + id - aInt) % 26
            position + aInt |> char

        let decryptedName = 
            room.EncryptedName 
            |> Seq.map (fun c ->
                match c with
                | '-' -> ' '
                | c when c >= 'a' && c <= 'z' -> shift c room.Id
                | _ -> failwithf "Unexpected character in name: %c" c)
            |> Seq.toArray
            |> System.String

        {
            DecryptedName = decryptedName
            Id = room.Id
        }

let exampleRoom = {
    EncryptedName = "qzmt-zixmtkozy-ivhz"
    Id = 343
    Checksum = "abxyz"
}

exampleRoom |> Room.decrypt
    //     room.EncryptedName
    //     |> Seq.map (fun )

let checksumMatch room =
    room.Checksum = Room.checksum room



let example = [
    "aaaaa-bbb-z-y-x-123[abxyz]"
    "a-b-c-d-e-f-g-h-987[abcde]"
    "not-a-real-room-404[oarel]"
    "totally-real-room-200[decoy]"
]

let rooms = example |> parse

rooms[1] |> Room.checksum

input
|> parse
|> List.filter checksumMatch
|> List.sumBy (fun room -> room.Id)

let firstStar =
    input
    |> parse
    |> List.filter checksumMatch
    |> List.sumBy (fun room -> room.Id)

firstStar

let secondStar = 
    
    let storage = 
        input
        |> parse
        |> List.filter checksumMatch
        |> List.map Room.decrypt
        |> List.find(fun room -> room.DecryptedName.Contains("north"))

    storage.Id


secondStar

