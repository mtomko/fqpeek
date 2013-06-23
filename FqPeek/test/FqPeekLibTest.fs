module FqPeekLib.Test

open FqPeek.Lib
open FsUnit
open NUnit.Framework
open System

[<Test>]
let TestToInt () =
    match "74" with
    | ToInt x -> Assert.AreEqual(74, x)
    | _ -> Assert.Fail("This should not be reached")

    match "seventy-four" with
    | ToInt x -> Assert.Fail("This case should not fire")
    | _ -> Assert.True(true)

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

