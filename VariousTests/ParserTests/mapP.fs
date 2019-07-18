module mapP

open FsCheck
open FsCheck.Xunit

open ParserTypes
open ParserMonad
open Parsers

open ParserTestsUtils

type ``-> mapP`` () =
    let doTest num str parserBuilder =
        let nums = num |> string
        let input =  nums + str

        let parser = parserBuilder nums

        let result = run parser input

        result
        |> checkMatched num

    [<Property>]
    member x.``SHOULD transform the result of the parser`` (num: int32) (s:NonEmptyString) =
        doTest num s.Get (fun s -> mapP (int) (pstring s))
        
    [<Property>]
    member x.``infix SHOULD transform the result of the parser`` (num: int32) (s:NonEmptyString) =
        doTest num s.Get (fun s -> int <!> (pstring s))

    [<Property>]
    member x.``infix inverted SHOULD transform the result of the parser`` (num: int32) (s:NonEmptyString) =
        doTest num s.Get (fun s -> (pstring s) |>> int)
