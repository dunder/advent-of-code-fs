// --- Day 7: No Space Left On Device ---

open System.IO
open System.Text.RegularExpressions

System.Environment.CurrentDirectory <- __SOURCE_DIRECTORY__

let input = File.ReadLines(@".\Input\Day07.txt") |> List.ofSeq

let example = 
    [
        "$ cd /"
        "$ ls"
        "dir a"
        "14848514 b.txt"
        "8504156 c.dat"
        "dir d"
        "$ cd a"
        "$ ls"
        "dir e"
        "29116 f"
        "2557 g"
        "62596 h.lst"
        "$ cd e"
        "$ ls"
        "584 i"
        "$ cd .."
        "$ cd .."
        "$ cd d"
        "$ ls"
        "4060174 j"
        "8033020 d.log"
        "5626152 d.ext"
        "7214296 k"
    ]

let (|Regex|_|) pattern input = 
    let m = Regex.Match(input, pattern)

    if m.Success
    then Some(List.tail [for g in m.Groups -> g.Value])
    else None

type Command = 
    | Change of string
    | MoveOut
    | MoveRoot
    | List

type FileSystemItem =
    | File of File
    | Directory of Directory
and File = {name:string; fileSize:int64}
and Directory = {name:string; content:FileSystemItem list}

type Output = 
    | Command of Command
    | DirectoryListing of FileSystemItem

type Path = string list

type FileSystem =
    {
        CurrentPath: string list
        Directories: Map<Path, list<FileSystemItem>>
    }

let root = Directory {name = "root"; content = []}

let parse (lines: string list) =

    let output line = 
        match line with
        | Regex "\$ cd /" [] -> Command(MoveRoot)
        | Regex "\$ cd \.\." [] -> Command(MoveOut)
        | Regex "\$ cd (.*)" [directory] -> Command(Change directory)
        | Regex "\$ ls" [] -> Command(List)
        | Regex "dir (.*)" [directory] -> DirectoryListing(Directory{name = directory; content = []})
        | Regex "(\d+) (.*)" [size; name] -> DirectoryListing(File {name = name; fileSize = size |> int64})
        | _ -> failwithf "Unkown output: %s" line

    lines |> List.map output


let changeDirectory directory (state: FileSystem) = 
    let newDirectory = Directory { name = directory; content = [] }
    {state with 
        CurrentPath = directory::state.CurrentPath
    }

let moveOut (state: FileSystem) =     
    {state with CurrentPath = state.CurrentPath.Tail}

let changeToRootDirectory (state: FileSystem) = {state with CurrentPath = ["root"]}

let listDirectory (state: FileSystem) = state

let applyCommand (state: FileSystem) (command: Command) = 

    let newState =
        match command with 
        | Change directory -> state |> changeDirectory directory
        | MoveOut -> state |> moveOut
        | MoveRoot -> state |> changeToRootDirectory
        | List -> state |> listDirectory

    newState
    
let addDirectoryContent (state: FileSystem) (content: FileSystemItem) = 
    
    let currentContent = 
        if state.Directories |> Map.containsKey state.CurrentPath then        
            state.Directories[state.CurrentPath]
        else
            []

    let updatedContent = state.Directories |> Map.add state.CurrentPath (content::currentContent)
    
    { state with
        Directories = updatedContent}

    

let buildFileSystem terminalOutput =

    let emptyFileSystem = 
        { 
            CurrentPath = []
            Directories = Map.empty
        }

    let nextState (state: FileSystem) line =
        match line with
        | Command command -> applyCommand state command
        | DirectoryListing listing -> addDirectoryContent state listing

    terminalOutput
    |> List.fold nextState emptyFileSystem


let size directory fileSystem =

    let rec loop d fs total =
        let content = fs.Directories[d]
        content 
        |> List.sumBy (fun c ->
            match c with
            | File file -> file.fileSize
            | Directory directory -> 
                let path = directory.name::d
                loop path fs total
        )
        |> (+) total


    loop directory fileSystem 0

let exampleFs = example |> parse |> buildFileSystem

exampleFs.Directories 
|> Map.keys 
|> Seq.map (fun path -> size path exampleFs)
|> Seq.filter (fun size -> size <= 100000)
|> Seq.sum


let inputFs = input |> parse |> buildFileSystem

inputFs.Directories 
|> Map.keys 
|> Seq.map (fun path -> size path inputFs)
|> Seq.filter (fun size -> size <= 100000)
|> Seq.sum

let firstStar =
    0

firstStar


let availableSpace = 70000000L
let unusedLimit = 30000000L

let sizeOfRoot = size ["root"] inputFs
let unused = availableSpace - sizeOfRoot

let leftToClean = unusedLimit - unused

inputFs.Directories 
|> Map.keys 
|> Seq.map (fun path -> size path inputFs)
|> Seq.filter (fun size -> size >= leftToClean)
|> Seq.min

let secondStar = 
    0

secondStar

