module Matrix

type Direction = North | NorthEeast | East | SouthEast | South | SouthWest | West | NorthWest

type Position = { X : int; Y : int } 

let allDirections = [North; NorthEeast; East; SouthEast; South; SouthWest; West; NorthWest]
let mainDirections = [North; East; South; West]

let moveN steps d p = 

    match d with
    | North      -> { X = p.X;         Y = p.Y + steps }
    | NorthEeast -> { X = p.X + steps; Y = p.Y + steps }
    | East       -> { X = p.X + steps; Y = p.Y         }
    | SouthEast  -> { X = p.X + steps; Y = p.Y - steps }
    | South      -> { X = p.X;         Y = p.Y - steps }
    | SouthWest  -> { X = p.X - steps; Y = p.Y - steps }
    | West       -> { X = p.X - steps; Y = p.Y         }
    | NorthWest  -> { X = p.X - steps; Y = p.Y + steps }

let move d p = moveN 1 d p

let manhattanDistance p1 p2 = abs (p2.X - p1.X) + abs (p2.Y - p1.Y) 

let adjacent directions p =
    directions |> Seq.map (fun d -> move d p)

let adjacentAllDirections p = adjacent allDirections p
let adjacentMainDirections p = adjacent mainDirections p

type Turn = Left | Right

let turn facing turning = 
    match (facing, turning) with
    | (North, Left) -> West
    | (North, Right) -> East
    | (East, Left) -> North
    | (East, Right) -> South
    | (South, Left) -> East
    | (South, Right) -> West
    | (West, Left) -> South
    | (West, Right) -> North
    | _ -> failwithf "Unsupported turn facing %O turning %O" facing turning


module Tests =

    open Xunit

    type ExpectedWhenMoveOneFromOrigo() as this = 
        inherit TheoryData<Direction, Position>()
        do 
            this.Add(Direction.North, { X = 0; Y = 1 })
            this.Add(Direction.NorthEeast, { X = 1; Y = 1 })
            this.Add(Direction.East, { X = 1; Y = 0 })
            this.Add(Direction.SouthEast, { X = 1; Y = -1 })
            this.Add(Direction.South, { X = 0; Y = -1 })
            this.Add(Direction.SouthWest, { X = -1; Y = -1 })
            this.Add(Direction.West, { X = -1; Y = 0 })
            this.Add(Direction.NorthWest, { X = -1; Y = 1 })

    [<Theory; ClassData(typeof<ExpectedWhenMoveOneFromOrigo>)>]
    let  ``move in all directions`` (direction: Direction, expectedPosition: Position) : unit =
        let x = -1
        let position = move direction { X=0;Y=0 }
        Assert.Equal(expectedPosition, position)

    type ExpectedWhenMove3FromOrigo() as this = 
        inherit TheoryData<Direction, Position>()
        do 
            this.Add(Direction.North, { X = 0; Y = 3 })
            this.Add(Direction.NorthEeast, { X = 3; Y = 3 })
            this.Add(Direction.East, { X = 3; Y = 0 })
            this.Add(Direction.SouthEast, { X = 3; Y = -3 })
            this.Add(Direction.South, { X = 0; Y = -3 })
            this.Add(Direction.SouthWest, { X = -3; Y = -3 })
            this.Add(Direction.West, { X = -3; Y = 0 })
            this.Add(Direction.NorthWest, { X = -3; Y = 3 })

    [<Theory; ClassData(typeof<ExpectedWhenMove3FromOrigo>)>]
    let  ``move 3 in all directions`` (direction: Direction, expectedPosition: Position) : unit =
        let position = moveN 3 direction { X=0;Y=0 }
        Assert.Equal(expectedPosition, position)

    type ExpectedManhattanDistance() as this = 
        inherit TheoryData<Position, Position, int>()
        do 
            this.Add({ X = 0; Y = 0 }, { X = 2; Y = 2 }, 4)

    [<Theory; ClassData(typeof<ExpectedManhattanDistance>)>]
    let ``manhattan distance tests`` (p1:Position) (p2:Position) (expected:int) : unit =
        let actual = manhattanDistance p1 p2
        Assert.Equal(expected, actual)
