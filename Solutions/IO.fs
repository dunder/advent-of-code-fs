module Aoc.IO

open Microsoft.FSharp.Quotations
open System.IO

let rec functionName = function
| Patterns.Call(None, methodInfo, _) -> methodInfo.Name
| Patterns.Lambda(_, expression) -> functionName expression
| _ -> failwith "Unexpected input"



let readLines filePath = File.ReadLines(filePath)
let readText filePath = File.ReadAllText(filePath)




