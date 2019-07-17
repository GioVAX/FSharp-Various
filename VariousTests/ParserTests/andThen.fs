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
        let result = failurexxx c1 c2

        result 
        |> checkFailure "No more input"

    [<Property>]
    member x.``when fails on the first parser SHOULD specify the failure was the first parser`` (c1:char) (c2:char) (str:NonWhiteSpaceString) =
        ((str.Get.Length > 0) && (c1 <> c2))
        ==> lazy
        let expectedMsg = sprintf "Expecting '%c'." c1
        
        let result = failure1stParse c1 c2 str.Get

        result |> checkFailure expectedMsg

    [<Property>]
    member x.``when fails on the second parser SHOULD specify the failure was the second parser`` (c1: char) (c2:char) (str:NonWhiteSpaceString) =
        (str.Get.Length > 0 && c1 <> c2)
        ==> lazy
        let expectedMsg = sprintf "Expecting '%c'." c2
        
        let result = failure2ndParse c1 c2 str.Get

        result |> checkFailure expectedMsg