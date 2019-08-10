module pstring

open FsCheck
open FsCheck.Xunit

open ParserTypes
open Parsers

open ParserTestsUtils

type ``pstring tests`` () =

    let successTest s1 s2 =
        let input = s1 + s2
        run (pstring s1) input

    [<Property>]
    member x.``a successful match SHOULD return the matched string`` (s1:NonEmptyString) (s2:NonEmptyString) =
        let s1' = s1.Get

        let result = successTest s1' s2.Get

        result |> checkMatched s1'

    [<Property>]
    member x.``a successful match SHOULD remove the matched string from the input`` (s1:NonEmptyString) (s2:NonEmptyString) =
        let s2' = s2.Get

        let result = successTest s1.Get s2'

        result |> checkRemaining s2'
