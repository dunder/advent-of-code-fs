module AoC.IO

open System
open System.IO

let path event day = sprintf @".\%s\Input\%s.txt" event day

let readInputLines event day = File.ReadLines(path event day)
let readInputText event day = File.ReadAllText(path event day)