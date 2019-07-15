module ParserTests

open Xunit
open FsCheck
open FsCheck.Xunit
open FsUnit.Xunit

open ParserTypes
open Parsers

let private checkFailure fcheck message = function
    | Failure msg -> msg |> should fcheck message
    | _ -> Assert.False( true, "This should have failed!")

[<Property>]
let ``Parsing anything from an empty string SHOULD fail`` (c: char) =
    let parser = pchar c;

    let result = run parser ""

    result 
    |> checkFailure equal "No more input"
    
[<Property>]
let ``Parsing for the correct first char SHOULD return success`` (str: NonEmptyString) =
    let s = str.Get
    let firstChar = s.[0]
    let parser = pchar firstChar;
    let expected = Success (firstChar, s.[1..])

    let result = run parser s
    result |> should equal expected

[<Property>]
let ``Parsing for the wrong first char SHOULD fail`` (str: NonEmptyString) =
    let s = str.Get
    let c = match s.[0] with
                    | 'a' -> 'b'
                    | _ -> 'a'
    let parser = pchar c
    let expectedMsg = sprintf "Expecting '%c'. Got " c 

    let result = run parser s

    result
    |> checkFailure startWith expectedMsg
