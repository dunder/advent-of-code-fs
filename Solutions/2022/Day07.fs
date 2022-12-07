module AoC.E2022.Day07

// --- Day 7: No Space Left On Device ---

open AoC
open IO
open System.Text.RegularExpressions

let input = readInputLines "2022" "Day07" |> List.ofSeq


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
and File = {name:string; fileSize:int}
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

let parse (lines: string list) =

    let output line = 
        match line with
        | Regex "\$ cd /" [] -> Command(MoveRoot)
        | Regex "\$ cd \.\." [] -> Command(MoveOut)
        | Regex "\$ cd (.*)" [directory] -> Command(Change directory)
        | Regex "\$ ls" [] -> Command(List)
        | Regex "dir (.*)" [directory] -> DirectoryListing(Directory{name = directory; content = []})
        | Regex "(\d+) (.*)" [size; name] -> DirectoryListing(File {name = name; fileSize = size |> int})
        | _ -> failwithf "Unkown output: %s" line

    lines |> List.map output

module FileSystem =

    let changeDirectory directory (state: FileSystem) = { state with CurrentPath = directory::state.CurrentPath }

    let moveOut (state: FileSystem) = {state with CurrentPath = state.CurrentPath.Tail}

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
        
        { state with Directories = updatedContent}

    let buildFileSystem terminalOutput =

        let emptyFileSystem = { CurrentPath = []; Directories = Map.empty }

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

    let directorySizes fileSystem =
        fileSystem.Directories
        |> Map.keys
        |> Seq.map (fun path -> size path fileSystem)


let firstStar () =    
    input 
    |> parse 
    |> FileSystem.buildFileSystem
    |> FileSystem.directorySizes
    |> Seq.filter (fun size -> size <= 100000)
    |> Seq.sum


let secondStar () = 
    
    let fileSystem =
        input 
        |> parse 
        |> FileSystem.buildFileSystem

    let availableSpace = 70000000
    let unusedLimit = 30000000

    let sizeOfRoot = FileSystem.size ["root"] fileSystem
    let unused = availableSpace - sizeOfRoot

    let leftToClean = unusedLimit - unused

    fileSystem
    |> FileSystem.directorySizes
    |> Seq.filter (fun size -> size >= leftToClean)
    |> Seq.min
