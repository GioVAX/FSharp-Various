module ParserTests

open Xunit
open FsCheck
open FsCheck.Xunit
open FsUnit.Xunit

open ParserTypes
open Parsers
open ParserUtils

let private checkFailure fcheck message = function
    | Failure msg -> msg |> should fcheck message
    | _ -> Assert.False( true, "This should have failed!")

let private checkSuccess fcheck = function
    | Success(c, s) -> fcheck c s
    | _ -> Assert.False( true, "This should have succeeded!")

[<Property>]
let ``Parsing anything from an empty string SHOULD fail`` (c: char) =
    let parser = pchar c;

    let result = run parser ""

    result 
    |> checkFailure equal "No more input"
    
type ``Parsing for the correct first char`` () =
    [<Property>]
    member x.``SHOULD return success`` (str: NonEmptyString) =
        let s = str.Get
        let firstChar = s.[0]
        let parser = pchar firstChar;

        let result = run parser s
        result |> checkSuccess (fun _ _ -> ())

    [<Property>]
    member x.``SHOULD return the matched char`` (str: NonEmptyString) =
        let s = str.Get
        let firstChar = s.[0]
        let parser = pchar firstChar;

        let result = run parser s
        result |> checkSuccess (fun c _ -> c |> should equal firstChar)

    [<Property>]
    member x.``SHOULD return the remaining input without the first char`` (str: NonEmptyString) =
        let s = str.Get
        match s |> Seq.toList with
        | [] -> failwith "This should have been a NonEmptyString!"
        | (firstChar::tail) ->
            let parser = pchar firstChar;

            let result = run parser s
            result |> checkSuccess (fun _ s -> s |> should equal (charListToString tail))

    [<Property>]
    member x.``from a one char input string SHOULD succeed and return an empty remaining input`` (c:char) =
        let s = [c] |> charListToString
        let parser = pchar c
        let expected = Success(c, "")

        let result = run parser s

        result |> should equal expected

[<Property>]
let ``Parsing for the wrong first char SHOULD fail`` (str: NonEmptyString) =
    let s = str.Get
    let c = match s.[0] with
                    | 'a' -> 'b'
                    | _ -> 'a'
    let parser = pchar c
    let expectedMsg = sprintf "Expecting '%c'. Got '%c'" c s.[0] 

    let result = run parser s

    result
    |> checkFailure startWith expectedMsg

