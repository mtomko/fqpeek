module FqPeek.Test

open NUnit.Framework
open FsUnit

open System
open FqPeek.Lib

[<Test>]
let TestPhredAtoi () =
    phredAtoi Sanger '@' |> should equal 31
    phredAtoi Illumina '@' |> should equal 0   
