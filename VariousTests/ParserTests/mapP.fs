module mapP

open FsCheck

open ParserTypes
open ParserMonad
open Parsers

open ParserTestsUtils

type ``-> mapP`` () =
    member x.``SHOULD transform the result of the parser`` (num: int32) (s:NonEmptyString) =
        let nums = num |> string
        let input =  nums + s.Get

        let parser = mapP (int) (pstring nums)

        let result = run parser input

        result
        |> checkMatched num
        
type ``-> mapP infix`` () =
    member x.``SHOULD transform the result of the parser`` (num: int32) (s:NonEmptyString) =
        let nums = num |> string
        let input =  nums + s.Get

        let parser = int <!> (pstring nums)

        let result = run parser input

        result
        |> checkMatched num

type ``-> mapP infix inverted`` () =
    member x.``SHOULD transform the result of the parser`` (num: int32) (s:NonEmptyString) =
        let nums = num |> string
        let input =  nums + s.Get

        let parser = (pstring nums) |>> int

        let result = run parser input

        result
        |> checkMatched num
