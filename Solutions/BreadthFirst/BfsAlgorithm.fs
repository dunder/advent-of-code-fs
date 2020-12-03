namespace BFS

// attempt to use Carsten König's AStar implementation as a basis for a BFS algorithm

type Path<'node> = 'node seq

module Algorithm =

    type Config<'node when 'node : comparison> =
        {
            neighbours : 'node -> 'node seq
            isGoal : 'node -> bool
        }

    type private Runtime<'node when 'node : comparison> =
        {
            neighbours : 'node -> 'node seq
            isGoal : 'node -> bool
            openNodes : queue<'node>
            visitedNodes : Set<'node>
            cameFrom : Map<'node,'node>
        }

    let private initRuntime (start : 'node) (config : Config<'node>) =
        {
            neighbours = config.neighbours
            isGoal = config.isGoal
            openNodes =  Queue.enqueue Queue.empty start
            visitedNodes = Set.empty
            cameFrom = Map.empty
        }

    let rec private reconstructPath' (acc : 'node list) (toNode : 'node) (runtime : Runtime<'node>) =
        match runtime.cameFrom.TryFind toNode with
        | None -> toNode :: acc
        | Some parent -> reconstructPath' (toNode :: acc) parent runtime


    let private reconstructPath (toNode : 'node) (runtime : Runtime<'node>) =
        reconstructPath' [] toNode runtime |> Seq.ofList

    let private processChild (node : 'node) (runtime : Runtime<'node>) (child : 'node) =
        { runtime with
            openNodes = Queue.enqueue runtime.openNodes child
            cameFrom = runtime.cameFrom.Add (child, node)
        }

    let rec private runAlgorithm (runtime : Runtime<'node>) =
        let current, openNodes = Queue.dequeue runtime.openNodes
        if runtime.isGoal current then
            runtime |> reconstructPath current
        else
            let open' = openNodes
            let visited' = runtime.visitedNodes |> Set.add current
            let runtime' = { runtime with openNodes = open'; visitedNodes = visited' }
            let children =
                runtime.neighbours current
                |> Seq.filter (visited'.Contains >> not)
            let runtime'' =
                children
                |> Seq.fold (processChild current) runtime'
            runAlgorithm runtime''

    let bfs (start : 'node) (config : Config<'node>) =
        config
        |> initRuntime start
        |> runAlgorithm
