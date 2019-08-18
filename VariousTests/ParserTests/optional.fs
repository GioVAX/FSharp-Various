module optional

open FsCheck
open FsCheck.Xunit

open ParserTypes
open Parsers

open ParserTestsUtils

type ``optional tests`` () =

    let successMatch c s =
        let input = sprintf "%c%s" c s
        let parser = c |> pchar |> optional

        run parser input

    [<Property>]
    member x.``a successful match SHOULD succeed and return Some match`` (c:char) (s:NonEmptyString) =
        let s' = s.Get

        s'.[0] <> c
        ==> lazy
        let result = successMatch c s'

        result |> checkMatched (Some c)

    [<Property>]
    member x.``a successful match SHOULD remove the match from input`` (c:char) (s:NonEmptyString) =
        let s' = s.Get

        s'.[0] <> c
        ==> lazy
        let result = successMatch c s'

        result |> checkRemaining s'

    [<Property>]
    member x.``a failed match SHOULD succeed and return None`` (c:char) (s:NonEmptyString) =
        let s' = s.Get

        s'.[0] <> c
        ==> lazy
        let parser = c |> pchar |> optional

        let result = run parser s'

        result |> checkMatched None
