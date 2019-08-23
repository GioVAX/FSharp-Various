module pint

open FsCheck
open FsCheck.Xunit

open ParserTypes
open Parsers
open ParserTestsUtils

type ``pint tests`` () =

    [<Property>]
    member x.``numbers SHOULD matched`` (n:int) (s:NonEmptyString) =
        let s' = s.Get

        ['0'..'9'] |> List.contains s'.[0] |> not
        ==> lazy
        let input = sprintf "%i%s" n s'
        
        let result = run pint input

        result |> checkMatched n


    [<Property>]
    member x.``numbers SHOULD be removed from input`` (n:int) (s:NonEmptyString) =
        let s' = s.Get

        ['0'..'9'] |> List.contains s'.[0] |> not
        ==> lazy
        let input = sprintf "%i%s" n s'
        
        let result = run pint input

        result |> checkRemaining s'

    [<Property>]
    member x.``lack of numbers SHOULD result in failure`` (s:NonEmptyString) =
        let s' = s.Get

        '-'::['0'..'9'] |> List.contains s'.[0] |> not
        ==> lazy
       
        let result = run pint s'

        result |> checkFailure "Unexpected"