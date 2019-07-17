module ParserTests

open FsCheck.Xunit

open ParserTypes
open Parsers

open ParserTestsUtils

[<Property>]
let ``Parsing anything from an empty string SHOULD fail for no more input`` (c: char) =
    let parser = pchar c;

    let result = run parser ""

    result 
    |> checkFailure "No more input"