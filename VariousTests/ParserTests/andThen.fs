module andThen

open FsCheck
open FsCheck.Xunit

open ParserTypes
open Parsers
open ParserUtils

open ParserTestsUtils

type ``-> concatenate 2 parsers`` () =

    let buildInput c1 c2 str =
        (Seq.append [c1;c2] str)
        |> Seq.toArray
        |> System.String

    let parse c1 c2 input = 
        let pc1 = pchar c1
        let pc2 = pchar c2

        run (pc1 .>>. pc2) input

    let successfulParse c1 c2 str =
        buildInput c1 c2 str
        |> parse c1 c2

    let failure1stParse c1 c2 str =
        buildInput c2 c2 str
        |> parse c1 c2

    let failure2ndParse c1 c2 str =
        buildInput c1 c1 str
        |> parse c1 c2

    let failureShort c1 c2 =
        [c1] 
        |> charListToString
        |> parse c1 c2

    [<Property>]
    member x.``when successful SHOULD return the 2 matches in a tuple`` (c1: char) (c2:char) (str:NonWhiteSpaceString) =
        let result = successfulParse c1 c2 str.Get
        result |> checkMatched (c1, c2)

    [<Property>]
    member x.``when successful SHOULD remove the matches from the returned input`` (c1: char) (c2:char) (str:NonWhiteSpaceString) =
        let s = str.Get
        let result = successfulParse c1 c2 s
        result |> checkRemaining s

    [<Property>]
    member x.``when input contains only the first parser SHOULD fail for no more input`` (c1: char) (c2:char) =
        failureShort c1 c2
        |> checkFailure "No more input"

    [<Property>]
    member x.``when fails on the first parser SHOULD specify the failure was the first parser`` (c1:char) (c2:char) (str:NonWhiteSpaceString) =
        ((str.Get.Length > 0) && (c1 <> c2))
        ==> lazy
        let expectedMsg = sprintf "Expecting '%c'." c1
        
        failure1stParse c1 c2 str.Get
        |> checkFailure expectedMsg

    [<Property>]
    member x.``when fails on the second parser SHOULD specify the failure was the second parser`` (c1: char) (c2:char) (str:NonWhiteSpaceString) =
        (str.Get.Length > 0 && c1 <> c2)
        ==> lazy
        let expectedMsg = sprintf "Expecting '%c'." c2
        
        failure2ndParse c1 c2 str.Get
        |> checkFailure expectedMsg