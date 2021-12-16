module AoC.E2021.Day16

// >>> insert day tagline here <<<

open AoC
open IO


let input = readInputText "2021" "Day16"


let toBinary text =

    let mappings = 
        [
            '0', "0000"
            '1', "0001"
            '2', "0010"
            '3', "0011"
            '4', "0100"
            '5', "0101"
            '6', "0110"
            '7', "0111"
            '8', "1000"
            '9', "1001"
            'A', "1010"
            'B', "1011"
            'C', "1100"
            'D', "1101"
            'E', "1110"
            'F', "1111"
        ] 
        |> Map.ofList

    text |> Seq.map (fun hex -> mappings.[hex]) |> String.concat ""

let binaryToInt (binaryChars: char seq) = 
    let binary = binaryChars |> System.String.Concat
    System.Convert.ToInt32(binary, 2)

let binaryToInt64 (binaryChars: char seq) = 
    let binary = binaryChars |> System.String.Concat
    System.Convert.ToInt64(binary, 2)

type Header = { Version: int; Id: int }

type OperatorType = | FixedBits | FixedCount

type PacketMeta = { Length: int; PackageCount: int; VersionSum: int }

type Packet = 
    | Literal of int64
    | Operator of OperatorType*int*list<Packet>

let rec readPacket (packet: string) =

    let readHeader (text: string) =

        let version = text |> Seq.take 3 |> binaryToInt
        let id = text |> Seq.skip 3 |> Seq.take 3 |> binaryToInt

        { Version = version; Id = id }

    let readLiteral (message: string) =
        let groupSize = 5

        let literalValue = 
            message 
            |> Seq.chunkBySize groupSize
            |> Seq.takeWhile (fun group-> group.[0] = '1')
            
        let groupCount = literalValue |> Seq.length
        let lastGroup = message |> Seq.chunkBySize groupSize |> Seq.skip groupCount |> Seq.take 1
        let literalValue = Seq.append literalValue lastGroup

        let value = 
            literalValue 
            |> Seq.map (fun group -> group.[1..]) 
            |> Seq.collect id 
            |> binaryToInt64

        let readLength = (groupCount + 1)*groupSize

        Literal (value), { Length = readLength; PackageCount = 1; VersionSum = 0 }

    let readOperatorFixedBits id (message: string) =

        let lengthBits = 15
        let subPacketsLength = System.Convert.ToInt32(message.[0..lengthBits-1], 2)

        let packets, _, versionSum =
            Seq.unfold (fun state -> 
                let packets, bitsRead, versionSum = state
                let packet, meta = readPacket message.[bitsRead..]
                let nextState = packet::packets, bitsRead + meta.Length, versionSum + meta.VersionSum
                Some (nextState, nextState)
            ) ([], lengthBits, 0)
            |> Seq.find (fun (_, bitsRead, _) -> 
                bitsRead = lengthBits + subPacketsLength)

        Operator (FixedBits, id, packets |> List.rev), { Length = subPacketsLength + lengthBits; PackageCount = packets |> Seq.length; VersionSum = versionSum }

    let readOperatorFixedCount id (message: string) =
        let lengthCount = 11
        let subPacketsCount = System.Convert.ToInt32(message.[0..lengthCount-1], 2)

        let packets, bitsRead, versionSum =
            [1..subPacketsCount]
            |> Seq.fold (fun state _ ->
                let packets, bitsRead, versionSum = state
                let packet, meta = readPacket message.[bitsRead..]
                let newPackets = packet::packets
                let newBitsRead = bitsRead + meta.Length
                let newVersionSum = versionSum + meta.VersionSum
                let nextState = newPackets, newBitsRead, newVersionSum
                nextState
            ) ([], lengthCount, 0)

        Operator (FixedCount, id, packets |> List.rev), { Length = bitsRead; PackageCount = subPacketsCount; VersionSum = versionSum }

    let readOperator id (message: string) =
        let lengthTypeId = message.[0]

        let payload = message.[1..]

        let packet, meta =
            match lengthTypeId with
            | '0' -> readOperatorFixedBits id payload
            | '1' -> readOperatorFixedCount id payload
            | _ -> failwithf "Unknown length type ID: %c" lengthTypeId
        
        packet, { meta with Length = 1 + meta.Length}

    let header = readHeader packet
    let message = packet.[6..]

    let package, meta =
        if header.Id = 4 then
            readLiteral message
        else 
            readOperator header.Id message

    package, { meta with Length = meta.Length + 6; PackageCount = meta.PackageCount + 1; VersionSum = meta.VersionSum + header.Version }


let firstStar () =
    let packet, meta = readPacket (input |> toBinary)
    meta.VersionSum


let rec evaluate packet = 
    
    match packet with
    | Literal x -> x
    | Operator (FixedBits, 0, packets) 
    | Operator (FixedCount, 0, packets) -> 
        packets |> Seq.map (fun packet -> evaluate packet) |> Seq.sum
    | Operator (FixedBits, 1, packets)
    | Operator (FixedCount, 1, packets) ->
        packets |> Seq.fold (fun product packet -> product * evaluate packet) 1L
    | Operator (FixedBits, 2, packets)
    | Operator (FixedCount, 2, packets) ->
        packets |> Seq.map (fun packet -> evaluate packet) |> Seq.min
    | Operator (FixedBits, 3, packets)
    | Operator (FixedCount, 3, packets) ->
        packets |> Seq.map (fun packet -> evaluate packet) |> Seq.max
    | Operator (FixedBits, 5, packets)
    | Operator (FixedCount, 5, packets) ->
        let packet1 = evaluate packets.[0]
        let packet2 = evaluate packets.[1]
        if packet1 > packet2 then 1L else 0L
    | Operator (FixedBits, 6, packets)
    | Operator (FixedCount, 6, packets) -> 
        let packet1 = evaluate packets.[0]
        let packet2 = evaluate packets.[1]
        if packet1 < packet2 then 1L else 0L
    | Operator (FixedBits, 7, packets)
    | Operator (FixedCount, 7, packets) ->
        let packet1 = evaluate packets.[0]
        let packet2 = evaluate packets.[1]
        if packet1 = packet2 then 1L else 0L
    | _ -> failwithf "Uncaught packet: %O" packet
    

let secondStar () = 
    let packet, meta = readPacket (input |> toBinary)
    evaluate packet
    