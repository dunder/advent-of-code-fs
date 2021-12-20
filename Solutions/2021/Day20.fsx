// --- Day 20: Trench Map ---

open System.IO

System.Environment.CurrentDirectory <- __SOURCE_DIRECTORY__

let input = File.ReadLines(@".\Input\Day20.txt") |> List.ofSeq

let example = 
    [
        "..#.#..#####.#.#.#.###.##.....###.##.#..###.####..#####..#....#..#..##..###..######.###...####..#..#####..##..#.#####...##.#.#..#.##..#.#......#.###.######.###.####...#.##.##..#..#..#####.....#.#....###..#.##......#.....#..#..#..##..#...##.######.####.####.#.#...#.......#..#.#.#...####.##.#......#..#...##.#.##..#...##.#.##..###.#......#.#.......#.#.#.####.###.##...#.....####.#..#..#.##.#....##..#.####....##...##..#...#......#.#.......#.......##..####..#...#.#.#...##..#.#..###..#####........#..####......#..#"
        ""
        "#..#."
        "#...."
        "##..#"
        "..#.."
        "..###"
    ]

let parseAlgorithm (lines: string list) = 
    lines.[0] 
    |> Seq.map (fun pixel ->
        let value =
            match pixel with
            | '#' -> true
            | '.' -> false
            | _ -> failwithf "Bad image data: %c" pixel

        value
    )
    |> List.ofSeq

let parseImage (lines: string list) =
    
    let imageLines = lines |> List.skip 2

    let rows = imageLines |> List.length
    let columns = imageLines.[0] |> Seq.length

    let image = Array2D.init rows columns (fun _ _ -> false)

    for row in 0..rows-1 do
        for column in 0..columns-1 do
            image.[row, column] <- 
                let pixel = imageLines.[row].[column]
                match pixel with
                | '#' -> true
                | '.' -> false
                | _ -> failwithf "Bad image data: %c" pixel

    image

let print (image:bool[,]) =
    let rows = image.GetLength(0)
    let columns = image.GetLength(1)

    printfn ""
    for row in 0..rows-1 do
        for column in 0..columns-1 do
            printf "%c" (if image.[row,column] then '#' else '.')
        printfn ""
    printfn ""

let enhance (algorithm:bool list) (inputImage:bool[,]) iteration =

    let inputRows = inputImage.GetLength(0)
    let inputColumns = inputImage.GetLength(1)
    
    let outOfBoundPixelState =
        if algorithm |> Seq.head && not (algorithm |> Seq.last) then
            // will toggle on each iteration 
            iteration % 2 = 0
        else 
            // will always be dark
            false

    let readImage = Array2D.init<bool> (inputRows+4) (inputColumns+4) (fun _ _ -> outOfBoundPixelState)

    let readImageRows = readImage.GetLength(0)
    let readImageColumns = readImage.GetLength(1)

    let writeImage = Array2D.zeroCreate<bool> (inputRows+2) (inputColumns+2) 

    // printfn "enhancing image %ix%i to %ix%i" inputRows inputColumns (writeImage.GetLength(0)) (writeImage.GetLength(1))
    // printfn "readImage: %ix%i" (readImage.GetLength(0)) (readImage.GetLength(1))
    // printfn "writeImage: %ix%i" (writeImage.GetLength(0)) (writeImage.GetLength(1))

    // copy the inputImage to the readImage
    for row in 0..inputRows-1 do
        for column in 0..inputColumns-1 do
            readImage.[row + 2, column + 2] <- inputImage.[row,column]

    // printfn "readImage:"
    // print readImage

    for row in 1..readImageRows-2 do
        for column in 1..readImageColumns-2 do
            let binary =
                seq {
                    for r in row-1..row+1 do
                        for c in column-1..column+1 do
                            readImage.[r, c]
                }
                |> Seq.map (fun value -> if value then '1' else '0')
                |> Array.ofSeq
                |> System.String.Concat
            let decimal = System.Convert.ToInt32(binary, 2)
            // printfn "(%i,%i): %s -> %i writing to: (%i, %i)" row column binary decimal (row-1) (column-1)
            writeImage.[row-1, column-1] <- algorithm.[decimal]

            
    writeImage


let allElements (a:'a[,]) =
    seq { 
        for row in 0 .. a.GetLength(0)-1 do
          for column in 0 .. a.GetLength(1)-1 do 
            yield (row, column), a.[row,column] 
    }

let allValues (a:'a[,]) = a |>allElements |> Seq.map snd

let countLights (image:bool[,]) = image |> allValues |> Seq.filter id |> Seq.length

let enhanceTimes (algorithm:bool list) (inputImage:bool[,]) iterations =

    [1..iterations]
    |> Seq.fold (fun inputImage iteration ->
        enhance algorithm inputImage iteration
    ) inputImage
    |> countLights


let firstStar =
    let data = input
    
    let algorithm = parseAlgorithm data
    
    let inputImage = parseImage data
    print inputImage

    let result = enhance algorithm inputImage 1
    print result

    let result2 = enhance algorithm result 2
    print result2
    countLights result2

    // 6315 too high, 5813 too high 5813
    

firstStar

let secondStar = 
    
    let data = input
    
    let algorithm = parseAlgorithm data
    
    let inputImage = parseImage data
    print inputImage

    let result = enhanceTimes algorithm inputImage 50
    result


secondStar

