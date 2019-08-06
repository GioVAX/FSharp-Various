module orElse

open FsCheck
open FsCheck.Xunit

open Parsers

open ParserTestsUtils
open CompositeUtils

let private successful1stParse composer c1 c2 str =
    buildInput c1 c2 str
    |> parse composer c1 c2

let private successful2ndParse composer c1 c2 str =
    buildInput c2 c1 str
    |> parse composer c1 c2

let private failingParse composer c1 c2 c3 str =
    buildInput c3 c3 str
    |> parse composer c1 c2

type ``-> prefix`` () =

    [<Property>]
    member x.``when first matches SHOULD return the match`` (c1:char) (c2:char) (str:NonWhiteSpaceString) =
        c1 <> c2
        ==> lazy
        let result = successful1stParse orElse c1 c2 str.Get
        result |> checkMatched c1

    [<Property>]
    member x.``when first matches SHOULD remove just one char`` (c1:char) (c2:char) (str:NonWhiteSpaceString) =
        c1 <> c2
        ==> lazy
        let s = str.Get
        let expected = (Seq.append [c2] s)
                        |> Seq.toArray
                        |> System.String

        let result = successful1stParse orElse c1 c2 s
        result |> checkRemaining expected

    [<Property>]
    member x.``when second matches SHOULD return the match`` (c1:char) (c2:char) (str:NonWhiteSpaceString) =
        c1 <> c2
        ==> lazy
        let result = successful2ndParse orElse c1 c2 str.Get
        result |> checkMatched c2

    [<Property>]
    member x.``when second matches SHOULD remove just one char`` (c1:char) (c2:char) (str:NonWhiteSpaceString) =
        c1 <> c2
        ==> lazy
        let s = str.Get
        let expected = (Seq.append [c1] s)
                        |> Seq.toArray
                        |> System.String

        let result = successful2ndParse orElse c1 c2 s
        result |> checkRemaining expected

    [<Property>]
    member x.``when neither match SHOULD specify the error was the second parser`` (c1:char) (c2:char) (c3:char) (str:NonWhiteSpaceString) =
        ((c1 <> c2) && (c1 <> c3) && (c2 <> c3))
        ==> lazy
        let expectedMsg = sprintf "Expecting '%c'." c2

        let result = failingParse orElse c1 c2 c3 str.Get
        result |> checkFailure expectedMsg

type ``-> infix`` () =

    [<Property>]
    member x.``when first matches SHOULD return the match`` (c1:char) (c2:char) (str:NonWhiteSpaceString) =
        c1 <> c2
        ==> lazy
        let result = successful1stParse (<|>) c1 c2 str.Get
        result |> checkMatched c1

    [<Property>]
    member x.``when first matches SHOULD remove just one char`` (c1:char) (c2:char) (str:NonWhiteSpaceString) =
        c1 <> c2
        ==> lazy
        let s = str.Get
        let expected = (Seq.append [c2] s)
                        |> Seq.toArray
                        |> System.String

        let result = successful1stParse (<|>) c1 c2 s
        result |> checkRemaining expected

    [<Property>]
    member x.``when second matches SHOULD return the match`` (c1:char) (c2:char) (str:NonWhiteSpaceString) =
        c1 <> c2
        ==> lazy
        let result = successful2ndParse (<|>) c1 c2 str.Get
        result |> checkMatched c2

    [<Property>]
    member x.``when second matches SHOULD remove just one char`` (c1:char) (c2:char) (str:NonWhiteSpaceString) =
        c1 <> c2
        ==> lazy
        let s = str.Get
        let expected = (Seq.append [c1] s)
                        |> Seq.toArray
                        |> System.String

        let result = successful2ndParse (<|>) c1 c2 s
        result |> checkRemaining expected

    [<Property>]
    member x.``when neither match SHOULD specify the error was the second parser`` (c1:char) (c2:char) (c3:char) (str:NonWhiteSpaceString) =
        ((c1 <> c2) && (c1 <> c3) && (c2 <> c3))
        ==> lazy
        let expectedMsg = sprintf "Expecting '%c'." c2

        let result = failingParse (<|>) c1 c2 c3 str.Get
        result |> checkFailure expectedMsg