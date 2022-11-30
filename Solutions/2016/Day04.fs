module AoC.E2016.Day04

// --- Day 4: Security Through Obscurity ---

open AoC
open IO


let input = readInputLines "2016" "Day04" |> List.ofSeq

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

    let checksumMatch room = room.Checksum = checksum room

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


let firstStar () =
    input
    |> parse
    |> List.filter Room.checksumMatch
    |> List.sumBy (fun room -> room.Id)


let secondStar () = 
    let storage = 
        input
        |> parse
        |> List.filter Room.checksumMatch
        |> List.map Room.decrypt
        |> List.find(fun room -> room.DecryptedName.Contains("north"))

    storage.Id