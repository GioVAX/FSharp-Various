module ParserTests

open FsCheck.Xunit
open FsUnit.Xunit

open ParserTypes
open ParserUtils
open Parsers

open ParserTestsUtils

[<Property>]
let ``Parsing anything from an empty string SHOULD fail for no more input`` (c: char) =
    let parser = pchar c;

    let result = run parser ""

    result 
    |> checkFailure "No more input"

[<Property>]
let ``run SHOULD call the wrapped function on input`` (s: string) =
    let f (s:string) = Success (s + s, "")

    run (Parser f) s
    |> checkMatched (s+s)

[<Property>]
let ``cons SHOULD return a new list with the new element at the head`` (head: char) (tail: char list) =
    let result = cons head tail
    let expected = head::tail

    result |> should equal expected