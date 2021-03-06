module FqPeek.Main

open System
open System.IO
open FqPeek.Count
open FqPeek.Lib

// the list of top-level commands available
type Command = Count | Filter | Help

// options for the count command
type CommandOption = MinLength of int | MaxLength of int

// contains all of the command line options so far
type CommandLineOptions = {
    command : Command;
    options : CommandOption list
    files : string list
}

let defaultOptions = { 
    command = Help; 
    options = []; 
    files = [] 
    }

let parseCommand (command : string) =
    match command.ToLower() with
    | "count" -> Count
    | "filter" -> Filter
    | _ -> Help

// based on http://fsharpforfunandprofit.com/posts/pattern-matching-command-line/
let rec parseCommandLineRec args optionsSoFar = 
    match args with 
    // empty list means we're done.
    | [] -> 
        optionsSoFar
    
    | "-h" :: xs ->
        let newOptionsSoFar = { optionsSoFar with command = Help }
        parseCommandLineRec xs newOptionsSoFar
    
    | "-m" :: xs ->
        // we need to extract at least one more argument
        match xs with
        | ToInt x :: xss ->
            let newOptionsSoFar = { optionsSoFar with options = (MaxLength x :: optionsSoFar.options) }
            parseCommandLineRec xss newOptionsSoFar
        | _ :: xss ->
            printfn "Minimum length requires an integer argument"
            parseCommandLineRec xss { optionsSoFar with command = Help }
        | [] ->
            printfn "Minimum length requires an integer argument"
            parseCommandLineRec [] { optionsSoFar with command = Help }
    
    | "-n" :: xs ->
        match xs with
        | ToInt x :: xss ->
            let newOptionsSoFar = { optionsSoFar with options = (MinLength x :: optionsSoFar.options) }
            parseCommandLineRec xss newOptionsSoFar
        | _ :: xss ->
            printfn "Maximum length requires an integer argument"
            parseCommandLineRec xss { optionsSoFar with command = Help }
        | [] ->
            printfn "Maximum length requires an integer argument"
            parseCommandLineRec [] { optionsSoFar with command = Help }
            
    | x :: xs ->
        // we assume that this is a file name
        parseCommandLineRec xs { optionsSoFar with files = x :: optionsSoFar.files }

// reads the option structure and returns a filter over fastq records
let buildFilterPredicate options = 
    // this could be reworked in terms of fold
    let rec loop (minLength : int option) (maxLength : int option) options = 
        match options with
        | [] -> (minLength, maxLength)
        | MinLength x :: xs -> loop (Some(x)) maxLength xs
        | MaxLength x :: xs -> loop minLength (Some(x)) xs
    match loop None None options with
    | Some(min), Some(max) -> 
        (fun (f : Fastq) -> let l = (Array.length f.Sequence) in l >= min && l <= max)
    | Some(min), None ->
        (fun (f : Fastq) -> let l = (Array.length f.Sequence) in l >= min)
    | None, Some(max) ->
        (fun (f : Fastq) -> let l = (Array.length f.Sequence) in l <= max)
    | None, None -> (fun f -> true)

let printMatchingReads predicate file =
    let records = filterFile predicate file
    for fastq in records do
        printfn "%s" (fastq.ToString())

let filterCommand options files =
    let predicate = buildFilterPredicate options
    if Seq.length files = 1 then
        printMatchingReads predicate (List.head files)
        0
    else
        -1

let printUsage() =
    printfn "Usage: seqpeek [count | filter | help] <options> <files>"
    0

// the main program
[<EntryPoint>]
let main (args : string[]) = 
    if args.Length < 1 then
        printUsage()
    else
        let command = parseCommand args.[0]
        let commandArgs : string list = Array.toList args.[1..]
        let commandLineOptions = parseCommandLineRec commandArgs { defaultOptions with command = command }
        match commandLineOptions.command with
        | Count -> FqPeek.Count.command commandArgs
        | Filter -> filterCommand commandLineOptions.options commandLineOptions.files
        | _ -> printUsage()

