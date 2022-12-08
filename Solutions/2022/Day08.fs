module AoC.E2022.Day08

// --- Day 8: Treetop Tree House ---

open AoC
open IO


let input = readInputLines "2022" "Day08" |> List.ofSeq


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

let atTopBorder (row, column) (forest:int[,]) = row = 0
let atLeftBorder (row, column) (forest:int[,]) = column = 0
let atBottomBorder (row, column) (forest:int[,]) = row = forest.GetLength(0)-1
let atRightBorder (row, column) (forest:int[,]) = column = forest.GetLength(1)-1

let atBorder (row, column) (forest:int[,]) = 
    atTopBorder (row, column) forest ||
    atLeftBorder (row, column) forest ||
    atBottomBorder (row, column) forest ||
    atRightBorder (row, column) forest

let lineOfSightUp (row, column) (a: int[,]) =
    if atTopBorder (row, column) a then 
        Seq.empty 
    else
        seq {
            for r in row-1..-1..0 do
                yield (r, column), a.[r, column] 
        }

let lineOfSightRight (row, column) (a: int[,]) =
    if atRightBorder (row, column) a then 
        Seq.empty
    else
        seq {
            for c in column+1..a.GetLength(0)-1 do
                yield (row, c), a[row, c] 
        }

let lineOfSightDown (row, column) (a: int[,]) =
    if atBottomBorder (row, column) a then  
        Seq.empty
    else
        seq {
            for r in row+1..a.GetLength(1)-1 do
                yield (r, column), a[r, column] 
        }  

let lineOfSightLeft (row, column) (a: int[,]) = 
    if atLeftBorder (row, column) a then
        Seq.empty
    else 
        seq {
            for c in column-1..-1..0 do
                yield (row, c), a[row, c] 
        }

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


let firstStar () =
    let inputForest = input |> parse

    input 
    |> parse 
    |> allElements 
    |> Seq.filter (fun (tree, height) -> visible tree inputForest) 
    |> Seq.length


let secondStar () = 
    let inputForest = input |> parse
    
    input 
    |> parse 
    |> allElements
    |> Seq.map (fun (tree, height) -> scenicScore tree inputForest) 
    |> Seq.max