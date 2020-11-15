module Matrix

type Direction = North | NorthEeast | East | SouthEast | South | SouthWest | West | NorthWest

type Position = { X : int; Y : int } 

let allDirections = [North; NorthEeast; East; SouthEast; South; SouthWest; West; NorthWest]
let mainDirections = [North; East; South; West]

let moveN p d steps = 

    match d with
    | North      -> { X = p.X;         Y = p.Y + steps }
    | NorthEeast -> { X = p.X + steps; Y = p.Y + steps }
    | East       -> { X = p.X + steps; Y = p.Y         }
    | SouthEast  -> { X = p.X + steps; Y = p.Y - steps }
    | South      -> { X = p.X;         Y = p.Y - steps }
    | SouthWest  -> { X = p.X - steps; Y = p.Y - steps }
    | West       -> { X = p.X - steps; Y = p.Y         }
    | NorthWest  -> { X = p.X - steps; Y = p.Y + steps }

let move p d = moveN p d 1

let adjacent directions p =
    directions |> Seq.map (fun d -> move p d)

let adjacentAllDirections p = adjacent allDirections p
let adjacentMainDirections p = adjacent mainDirections p


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
        let position = move { X=0;Y=0} direction
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
        let position = moveN { X=0;Y=0} direction 3
        Assert.Equal(expectedPosition, position)
