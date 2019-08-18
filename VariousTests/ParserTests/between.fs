module between

open FsCheck
open FsCheck.Xunit

open ParserTypes
open Parsers

open ParserTestsUtils

type ``between tests`` () =

    let buildInput = sprintf "%c%c%c%s"

    let successfulMatch before btwin after s =
        let input = buildInput before btwin after s
        let parser = between (pchar before) (pchar btwin) (pchar after)

        run parser input

    [<Property>]
    member x.``successful match SHOULD return only the middle match`` (before:char) (after:char) (btwin:char) (s:NonEmptyString) =
        let s' = s.Get

        s'.[0] <> after
        ==> lazy
        let result = successfulMatch before btwin after s'

        result |> checkMatched btwin

    [<Property>]
    member x.``successful match SHOULD remove all parts from input`` (before:char) (after:char) (btwin:char) (s:NonEmptyString) =
        let s' = s.Get

        s'.[0] <> after
        ==> lazy
        let result = successfulMatch before btwin after s'

        result |> checkRemaining s'

    [<Property>]
    member x.``lack of the initial match SHOULD fail with the correct message`` (before:char) (after:char) (btwin:char) (s:NonEmptyString) =
        let s' = s.Get

        before <> after
        ==> lazy
        let input = buildInput after btwin after s'
        let parser = between (pchar before) (pchar btwin) (pchar after)
        let expected = sprintf "Expecting '%c'." before

        let result = run parser input

        result |> checkFailure expected

    [<Property>]
    member x.``lack of the second match SHOULD fail with the correct message`` (before:char) (after:char) (btwin:char) (s:NonEmptyString) =
        let s' = s.Get

        btwin <> after
        ==> lazy
        let input = buildInput before after after s'
        let parser = between (pchar before) (pchar btwin) (pchar after)
        let expected = sprintf "Expecting '%c'." btwin

        let result = run parser input

        result |> checkFailure expected

    [<Property>]
    member x.``lack of the third match SHOULD fail with the correct message`` (before:char) (after:char) (btwin:char) (s:NonEmptyString) =
        let s' = s.Get

        btwin <> after
        ==> lazy
        let input = buildInput before btwin btwin s'
        let parser = between (pchar before) (pchar btwin) (pchar after)
        let expected = sprintf "Expecting '%c'." after

        let result = run parser input

        result |> checkFailure expected

    [<Property>]
    member x.``lack of all the matches SHOULD fail for the first match`` (before:char) (after:char) (btwin:char) (s:NonEmptyString) =
        let s' = s.Get

        before <> s'.[0]
        ==> lazy
        let parser = between (pchar before) (pchar btwin) (pchar after)
        let expected = sprintf "Expecting '%c'." before

        let result = run parser s'

        result |> checkFailure expected
