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
let ``Parsing anything from an empty string SHOULD fail for no more input`` (c: char) =
    let parser = pchar c;

    let result = run parser ""

    result 
    |> checkFailure equal "No more input"
    
type ``Parsing for the correct first char`` () =
    [<Property>]
    member x.``SHOULD return success`` (str: NonWhiteSpaceString ) =
        let s = str.Get
        let firstChar = s.[0]
        let parser = pchar firstChar;

        let result = run parser s
        result |> checkSuccess (fun _ _ -> ())

    [<Property>]
    member x.``SHOULD return the matched char`` (str: NonWhiteSpaceString ) =
        let s = str.Get
        let firstChar = s.[0]
        let parser = pchar firstChar;

        let result = run parser s
        result |> checkMatched firstChar

    [<Property>]
    member x.``SHOULD return the remaining input without the first char`` (str: NonWhiteSpaceString ) =
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
let ``Parsing for the wrong first char SHOULD fail`` (str: NonWhiteSpaceString ) =
    let s = str.Get
    let c = match s.[0] with
                    | 'a' -> 'b'
                    | _ -> 'a'
    let parser = pchar c
    let expectedMsg = sprintf "Expecting '%c'. Got '%c'" c s.[0] 

    let result = run parser s

    result
    |> checkFailure startWith expectedMsg

type ``Concatenate 2 parsers`` () =
    let buildInput c1 c2 str =
        (Seq.append [c1;c2] str)
        |> Seq.toArray
        |> System.String

    let successfulParse c1 c2 str =
        let input = buildInput c1 c2 str
        let pc1 = pchar c1
        let pc2 = pchar c2

        run (pc1 .>>. pc2) input

    let failure1stParse c1 c2 str =
        let input = buildInput c2 c2 str
        let pc1 = pchar c1
        let pc2 = pchar c2
        run (pc1 .>>. pc2) input

    let failure2ndParse c1 c2 str =
        let input = buildInput c1 c1 str
        let pc1 = pchar c1
        let pc2 = pchar c2
        run (pc1 .>>. pc2) input

    let failurexxx c1 c2 =
        let input = [c1] |> charListToString
        let pc1 = pchar c1
        let pc2 = pchar c2
        run (pc1 .>>. pc2) input

    [<Property>]
    member x.``when successful SHOULD return the 2 matches in a tuple`` (c1: char, c2:char, str:NonWhiteSpaceString) =
        let result = successfulParse c1 c2 str.Get
        result |> checkMatched (c1, c2)

    [<Property>]
    member x.``when successful SHOULD remove the matches from the returned input`` (c1: char, c2:char, str:NonWhiteSpaceString) =
        let s = str.Get
        let result = successfulParse c1 c2 s
        result |> checkRemaining s

    [<Property>]
    member x.``when input contains only the first parser SHOULD fail for no more input`` (c1: char, c2:char) =
        let result = failurexxx c1 c2

        result 
        |> checkFailure equal "No more input"

    [<Property>]
    member x.``when fails on the first parser SHOULD specify the failure was the first parser`` (c1: char, c2:char, str:NonWhiteSpaceString) =
        (str.Get.Length > 1 && c1 <> c2)
        ==> 
        let expectedMsg = sprintf "Expecting '%c'." c1
        
        let result = failure1stParse c1 c2 str.Get

        result |> checkFailure startWith expectedMsg

    [<Property>]
    member x.``when fails on the second parser SHOULD specify the failure was the second parser`` (c1: char, c2:char, str:NonWhiteSpaceString) =
        (str.Get.Length > 1 && c1 <> c2)
        ==> 
        let expectedMsg = sprintf "Expecting '%c'." c2
        
        let result = failure2ndParse c1 c2 str.Get

        result |> checkFailure startWith expectedMsg