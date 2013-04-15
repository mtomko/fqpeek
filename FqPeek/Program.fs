module FqPeek.Main

open System
open System.IO

// define the FASTQ data structure
[<Struct>]
type Fastq(id : String, sequence : char[], qual : char[]) = 
    member this.Id = id
    member this.Sequence = sequence
    member this.Qual = qual
   
    override this.ToString() =
        let seqs = String.Join("", sequence)
        let quals = String.Join(" ", qual)
        sprintf "@%s\n%s\n+%s\n%s" id seqs id quals

// the list of top-level commands available
type Command = Count | Help

// options for the count command
type CommandOption = MinLength of int | MaxLength of int

// contains all of the command line options so far
type CommandLineOptions = {
    command : Command;
    options : CommandOption list
    files : string list
}

let defaultOptions = { command = Help; options = []; files = [] }

// converts an string to an integer
// see Programming F# 3.0, pp. 188-9
let (|ToInt|_|) x =
    let success, result = Int32.TryParse(x)
    if success then Some(result)
    else None

let parseCommand (command : string) =
    match command.ToLower() with
    | "count" -> Count
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
            let newOptionsSoFar = { optionsSoFar with options = (MinLength x :: optionsSoFar.options) }
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
            let newOptionsSoFar = { optionsSoFar with options = (MaxLength x :: optionsSoFar.options) }
            parseCommandLineRec xss newOptionsSoFar
        | _ :: xss ->
            printfn "Maximum length requires an integer argument"
            parseCommandLineRec xss { optionsSoFar with command = Help }
        | [] ->
            printfn "Minimum length requires an integer argument"
            parseCommandLineRec [] { optionsSoFar with command = Help }
            
    | x :: xs ->
        // we assume that this is a file name
        parseCommandLineRec xs { optionsSoFar with files = x :: optionsSoFar.files }

// reads a file as a stream of Fastq records
let readFastq2 filePath = 
    let lineSeq = File.ReadLines(filePath)
    seq {
    while not (Seq.isEmpty lineSeq) do
        let record = Seq.toArray (Seq.take 4 lineSeq)
        let id = record.[0].Substring(1)
        let bases = record.[1].ToCharArray()
        let quals = record.[3].ToCharArray()
        yield new Fastq(id, bases, quals)
    }

let readFastq (filePath : string) =
    seq {
        use sr = new StreamReader (filePath)
        while not sr.EndOfStream do
            let id = sr.ReadLine().Substring(1)
            let bases = sr.ReadLine().ToCharArray()
            let id2 = sr.ReadLine()
            let quals = sr.ReadLine().ToCharArray()
            yield new Fastq(id, bases, quals)
    }

// reads the option structure and returns a filter over fastq records
let buildCountFilter options = 
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
    
let countMatchingReads countFilter file = 
    readFastq file
    |> Seq.filter countFilter
    |> Seq.length

let countCommand options files =
    let countFilter = buildCountFilter options
    if Seq.length files > 1 then
        for file in files do
            let count = countMatchingReads countFilter file
            printfn "%s\n\t%d" file count
    else
        let count = countMatchingReads countFilter (List.head files)
        printfn "%d" count
    0

// the main program
[<EntryPoint>]
let main (args : string[]) = 
    if args.Length < 1 then
        failwith "Usage: seqpeek <command> <options> <files>"
    
    let command = parseCommand args.[0]
    let commandLineOptions = parseCommandLineRec (Array.toList args.[1..]) { defaultOptions with command = command }
    match commandLineOptions.command with
    | Count -> countCommand commandLineOptions.options commandLineOptions.files
    | _ -> failwith "Usage: seqpeek <command> <options> <files>"

