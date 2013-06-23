module FqPeek.Count

open FqPeek.Lib
open Microsoft.FSharp.Text
open System

let minLength : int option ref = ref None
let maxLength : int option ref = ref None
let files : string list ref = ref []

let buildFileList filename =
    files := filename :: !files

let specs =
    [ "-m", ArgType.Int (fun i -> minLength := Some i), "Minimum read length";
      "-n", ArgType.Int (fun i -> maxLength := Some i), "Maximum read length";
      "--", ArgType.Rest buildFileList, "Files to process"] |> List.map (fun (sh, ty, desc) -> ArgInfo(sh, ty, desc))

let predicate =
    match (!minLength, !maxLength) with
    | Some(min), Some(max) -> 
        (fun (f : Fastq) -> let l = (Array.length f.Sequence) in l >= min && l <= max)
    | Some(min), None -> (fun (f : Fastq) -> let l = (Array.length f.Sequence) in l >= min)
    | None, Some(max) -> (fun (f : Fastq) -> let l = (Array.length f.Sequence) in l <= max)
    | None, None -> (fun f -> true)
    
let countMatchingReads predicate file = 
    filterFile predicate file |> Seq.length

let command (args : string list) = 
    ArgParser.Parse specs
    if Seq.length !files > 1 then
        for file in !files do
            let count = countMatchingReads predicate file
            printfn "%s\n\t%d" file count
    else
        let count = countMatchingReads predicate (List.head !files)
        printfn "%d" count
    0


    