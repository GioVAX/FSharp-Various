module ParserTests

open FsCheck
open FsCheck.Xunit
open FsUnit.Xunit

open ParserTypes
open Parsers
open ParserUtils

let private checkFailure fcheck message = function
    | Failure msg -> msg |> should fcheck message
    | _ -> failwith "This should have failed!"

let private checkSuccess fcheck = function
    | Success(matched, remainingInput) -> fcheck matched remainingInput
    | _ -> failwith "This should have succeeded!"

let private checkMatched expected = checkSuccess (fun res _ -> res |> should equal expected)
let private checkRemaining expected = checkSuccess (fun _ input -> input |> should equal expected)

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
        result |> checkMatched firstChar

    [<Property>]
    member x.``SHOULD return the remaining input without the first char`` (str: NonEmptyString) =
        let s = str.Get
        match s |> Seq.toList with
        | [] -> failwith "This should have been a NonEmptyString!"
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

[<Property>]
let ``Concatenating 2 parser chars SHOULD match the 2 chars when the input starts with those chars`` (c1: char, c2:char, str:NonEmptyString) =
    let input = (Seq.append [c1;c2] str.Get)
                |> Seq.toArray
                |> System.String
    let pc1 = pchar c1
    let pc2 = pchar c2

    let result = run (pc1 .>>. pc2) input

    result |> checkMatched (c1, c2)