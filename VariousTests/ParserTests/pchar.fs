module pchar

open FsCheck
open FsCheck.Xunit
open FsUnit.Xunit

open ParserTypes
open Parsers
open ParserUtils

open ParserTestsUtils

type ``-> parse one char`` () =
    [<Property>]
    member x.``SHOULD return success`` (str: NonWhiteSpaceString) =
        let s = str.Get
        let firstChar = s.[0]
        let parser = pchar firstChar;

        let result = run parser s
        result |> checkSuccess (fun _ _ -> ())

    [<Property>]
    member x.``SHOULD return the matched char`` (str: NonWhiteSpaceString) =
        let s = str.Get
        let firstChar = s.[0]
        let parser = pchar firstChar;

        let result = run parser s
        result |> checkMatched firstChar

    [<Property>]
    member x.``SHOULD return the remaining input without the first char`` (str: NonWhiteSpaceString) =
        let s = str.Get
        match s |> Seq.toList with
        | [] -> failwith "This should have been a NonWhiteSpaceString !"
        | (firstChar::tail) ->
            let parser = pchar firstChar;

            let result = run parser s
            result |> checkRemaining (charListToString tail)

    [<Property>]
    member x.``from a one char input string SHOULD succeed and return an empty remaining input`` (c:char) =
        let s = [c] |> charListToString
        let parser = pchar c
        let expected = Success(c, "")

        let result = run parser s

        result |> should equal expected

    [<Property>]
    member x.``Parsing for the wrong first char SHOULD fail`` (str: NonWhiteSpaceString) =
        let s = str.Get
        let c = match s.[0] with
                        | 'a' -> 'b'
                        | _ -> 'a'
        let parser = pchar c

        let result = run parser s

        result
        |> checkFailure "Unexpected"

    [<Property>]
    member x.``Parsing for the wrong first char SHOULD return the expected char`` (str: NonWhiteSpaceString) =
        let s = str.Get
        let c = match s.[0] with
                        | 'a' -> 'b'
                        | _ -> 'a'
        let parser = pchar c

        let result = run parser s

        result
        |> checkFailure "Unexpected"
