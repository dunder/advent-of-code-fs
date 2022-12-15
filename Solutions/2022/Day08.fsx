// --- Day 8: Treetop Tree House ---

open System.IO

System.Environment.CurrentDirectory <- __SOURCE_DIRECTORY__

let input = File.ReadLines(@".\Input\Day08.txt") |> List.ofSeq

let example = 
    [
        "30373"
        "25512"
        "65332"
        "33549"
        "35390"
    ]

let inline charToInt c = int c - int '0'

let parse (input: string list) =
    let data = 
        input
        |> Seq.map (fun line -> line |> Seq.map charToInt |> Seq.toList)
        |> Seq.toList

    let rows = input.Length
    let columns = input.Head.Length

    Array2D.init rows columns (fun x y -> data[x][y])

let allElements (a:'a[,]) =
    seq { 
        for row in 0 .. a.GetLength(0)-1 do
          for column in 0 .. a.GetLength(1)-1 do 
            yield (row, column), a[row,column] 
    }

let lineOfSightUp (row, column) (a: int[,]) =
    if row = 0 then 
        Seq.empty 
    else
        seq {
            for r in row-1..-1..0 do
                yield (r, column), a.[r, column] 
        }

let lineOfSightRight (row, column) (a: int[,]) =
    if column = a.GetLength(1)-1 then 
        Seq.empty
    else
        seq {
            for c in column+1..a.GetLength(0)-1 do
                yield (row, c), a[row, c] 
        }

let lineOfSightDown (row, column) (a: int[,]) =
    if row = a.GetLength(0)-1 then  
        Seq.empty
    else
        seq {
            for r in row+1..a.GetLength(1)-1 do
                yield (r, column), a[r, column] 
        }  

let lineOfSightLeft (row, column) (a: int[,]) = 
    if column = 0 then
        Seq.empty
    else 
        seq {
            for c in column-1..-1..0 do
                yield (row, c), a[row, c] 
        }

let forest = example |> parse

lineOfSightRight (0,4) forest

let atBorder (row, column) (forest:int[,]) =    
    row = 0 || row = forest.GetLength(1)-1 ||
    column = 0 || column = forest.GetLength(0)-1

let allLinesOfSight (tree:int*int) (forest:int[,]) =
    seq {
        yield lineOfSightUp tree forest
        yield lineOfSightRight tree forest
        yield lineOfSightDown tree forest
        yield lineOfSightLeft tree forest
    }

let visibleInLineOfSight ((tree:int*int)) (forest:int[,]) lineOfSight = 
    let row, column = tree
    let treeHeight = forest[row, column]
    lineOfSight |> Seq.forall (fun height -> height < treeHeight)


let visible (tree:int*int) (forest:int[,]) =

    let row, column = tree
    let height = forest[row,column]

    let linesOfSight = allLinesOfSight tree forest

    linesOfSight
    |> Seq.exists (fun lineOfSight ->
        lineOfSight |> Seq.map snd |> Seq.forall (fun h -> h < height)
    )
    
example 
|> parse 
|> allElements 
|> Seq.filter (fun (tree, height) -> atBorder tree forest) 
|> Seq.length


let printLineOfSight direction  (tree:int*int) (forest:int[,]) lineOfSight =
    let treeHeightsInLineOfSight = lineOfSight tree forest |> Seq.map snd
    let visible = visibleInLineOfSight tree forest treeHeightsInLineOfSight
    printfn "%s: %A, visible: %b" direction treeHeightsInLineOfSight visible

let test = 1,2

let alternateLineOfSightUp (row, column) (forest:int[,]) =
    forest[0..row-1, column] |> Array.rev

let alternateLineOfSightRight (row, column) (forest:int[,]) =
    forest[row, column+1..forest.GetLength(1)]


alternateLineOfSightRight (1,2) forest

printLineOfSight "up" test forest lineOfSightUp
printLineOfSight "right" test forest lineOfSightRight
printLineOfSight "down" test forest lineOfSightDown
printLineOfSight "left" test forest lineOfSightLeft
visible test forest

seq {

    for c in 1..0 do
        yield c
}

lineOfSightRight test forest

let inputForest = input |> parse

input 
|> parse 
|> allElements 
|> Seq.filter (fun (tree, height) -> visible tree inputForest) 
|> Seq.length

let treeCount lineOfSight tree (forest: int[,]) =

    let treeRow, treeColumn = tree
    let treeHeight = forest[treeRow, treeColumn]

    let inLineOfSight = lineOfSight tree forest |> Seq.map snd

    let found = inLineOfSight |> Seq.tryFindIndex (fun height -> height >= treeHeight)
    
    match found with
    | Some index -> inLineOfSight |> Seq.take (index+1) |> Seq.length
    | None -> inLineOfSight |> Seq.length
    
let scenicScore tree (forest: int[,]) =
    treeCount lineOfSightUp tree forest *
    treeCount lineOfSightRight tree forest *
    treeCount lineOfSightDown tree forest *
    treeCount lineOfSightLeft tree forest

let aTestTree = 1,2
treeCount lineOfSightDown aTestTree forest
scenicScore aTestTree forest

let allTrees = input |> parse |> allElements

allTrees |> Seq.map (fun (tree, height) -> scenicScore tree inputForest) |> Seq.max


let firstStar =
    0

firstStar

let secondStar = 
    0

secondStar

