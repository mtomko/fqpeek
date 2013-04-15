module FqPeekLib.Fastq

open System
open System.IO

type Dialect = Illumina | Solexa

[<Struct>]
type Fastq(id : String, sequence : char[], qual : char[]) = 
    member this.Id = id
    member this.Sequence = sequence
    member this.Qual = qual
   
    override this.ToString() =
        let seqs = String.Join("", sequence)
        let quals = String.Join(" ", qual)
        sprintf "@%s\n%s\n+%s\n%s" id seqs id quals

// reads a file as a stream of Fastq records
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