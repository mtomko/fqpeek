module FqPeek.Test

open NUnit.Framework
open FsUnit

open System
open FqPeek.Lib

[<Test>]
let TestPhredAtoI () =
    phredAtoI Sanger '@' |> should equal 31
    phredAtoI Illumina '@' |> should equal 0

[<Test>]
let TestPhredItoA () =
    phredItoA Sanger 31 |> should equal '@'

[<Test>]
let TestPhredRoundtrip () =
    let AtoI = phredAtoI Sanger
    let ItoA = phredItoA Sanger
    
    64 |> ItoA |> AtoI |> should equal 64

[<Test>]
let TestFastqToString () =
    let f = new Fastq("MD03", "AAUAAA".ToCharArray(), "99;999".ToCharArray())
    f.ToString() |> should equal "@MD03\nAAUAAA\n+MD03\n99;999"

