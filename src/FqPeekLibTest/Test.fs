module FqPeekLibTest

open System
open NUnit.Framework
open FqPeekLib.Fastq

[<Test>]
let PhredAtoiTest() =
    let atScore = phredAtoi IonTorrent '@'
    Assert.IsTrue(atScore.Equals(31))
