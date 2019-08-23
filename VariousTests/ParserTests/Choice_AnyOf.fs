module choice_anyOf

open FsCheck
open FsCheck.Xunit

open ParserTypes
open Parsers

open ParserTestsUtils

let rand = new System.Random()

let parserList f goodChar badChar numParsers =
    let pos = rand.Next(numParsers)
    [0..numParsers] 
        |> List.map (fun n -> if n = pos then goodChar else badChar )
        |> f

type ``choice tests`` () =
    
    let parsersBuilder = parserList (List.map pchar)

    [<Property>]
    member x.``a good match in a list of bad matches SHOULD succeed`` (p:char) (s:NonEmptyString) (numParsers:PositiveInt) =
        let str = s.Get

        str |> Seq.exists (fun c' -> c' = p) |> not
        ==> lazy
            let parsers = parsersBuilder p str.[0] numParsers.Get
            let input = sprintf "%c%s" p str

            let result = run (Parsers.choice parsers) input

            result |> checkMatched p

    [<Property>]
    member x.``no good match in a list of bad matches SHOULD fail`` (p:char) (s:NonEmptyString) (numParsers:PositiveInt) =
        let str = s.Get

        str |> Seq.exists (fun c' -> c' = p) |> not
        ==> lazy
        let first = str.[0]
        let parsers = parsersBuilder first first numParsers.Get
        let input = sprintf "%c%s" p str

        let result = run (Parsers.choice parsers) input
        
        result |> checkFailure "Unexpected"

type ``anyOf test`` () =

    let parsersBuilder = parserList id

    [<Property>]
    member x.``a good match in a list of bad matches SHOULD succeed`` (p:char) (s:NonEmptyString) (numParsers:PositiveInt) =
        let str = s.Get

        str |> Seq.exists (fun c' -> c' = p) |> not
        ==> lazy
            let parsers = parsersBuilder p str.[0] numParsers.Get
            let input = sprintf "%c%s" p str

            let result = run (Parsers.anyOf parsers) input
        
            result |> checkMatched p

    [<Property>]
    member x.``no good match in a list of bad matches SHOULD fail`` (p:char) (s:NonEmptyString) (numParsers:PositiveInt) =
        let str = s.Get

        str |> Seq.exists (fun c' -> c' = p) |> not
        ==> lazy
        let first = str.[0]
        let parsers = parsersBuilder first first numParsers.Get
        let input = sprintf "%c%s" p str

        let result = run (Parsers.anyOf parsers) input
        
        result |> checkFailure "Unexpected"
