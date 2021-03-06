﻿module concatenateP

open FsCheck
open FsCheck.Xunit

open ParserUtils
open ParserMonad
open Parsers

open ParserTestsUtils
open CompositeUtils

let private successfulParse composer c1 c2 str =
    buildInput c1 c2 str
    |> parse composer c1 c2

let private failure1stParse composer c1 c2 str =
    buildInput c2 c2 str
    |> parse composer c1 c2

let private failure2ndParse composer c1 c2 str =
    buildInput c1 c1 str
    |> parse composer c1 c2

let private failureShort composer c1 c2 =
    [c1] 
    |> charListToString
    |> parse composer c1 c2

type ``-> prefix`` () =
    [<Property>]
    member x.``when successful SHOULD return the 2 matches in a tuple`` (c1: char) (c2:char) (str:NonWhiteSpaceString) =
        let result = successfulParse concatenateP c1 c2 str.Get
        result |> checkMatched (c1, c2)

    [<Property>]
    member x.``when successful SHOULD remove both matches from the returned input`` (c1: char) (c2:char) (str:NonWhiteSpaceString) =
        let s = str.Get
        let result = successfulParse concatenateP c1 c2 s
        result |> checkRemaining s

    [<Property>]
    member x.``when input contains only the first parser SHOULD fail for no more input`` (c1: char) (c2:char) =
        failureShort concatenateP c1 c2
        |> checkFailure "No more input"

    [<Property>]
    member x.``when fails on the first parser SHOULD specify the failure was the first parser`` (c1:char) (c2:char) (str:NonWhiteSpaceString) =
        ((str.Get.Length > 0) && (c1 <> c2))
        ==> lazy
        
        failure1stParse concatenateP c1 c2 str.Get
        |> checkFailure "Unexpected"

    [<Property>]
    member x.``when fails on the second parser SHOULD specify the failure was the second parser`` (c1: char) (c2:char) (str:NonWhiteSpaceString) =
        (str.Get.Length > 0 && c1 <> c2)
        ==> lazy
        
        failure2ndParse concatenateP c1 c2 str.Get
        |> checkFailure  "Unexpected"

type ``-> infix`` () =
    [<Property>]
    member x.``when successful SHOULD return the 2 matches in a tuple`` (c1: char) (c2:char) (str:NonWhiteSpaceString) =
        let result = successfulParse (.>>.) c1 c2 str.Get
        result |> checkMatched (c1, c2)

    [<Property>]
    member x.``when successful SHOULD remove the matches from the returned input`` (c1: char) (c2:char) (str:NonWhiteSpaceString) =
        let s = str.Get
        let result = successfulParse (.>>.) c1 c2 s
        result |> checkRemaining s

    [<Property>]
    member x.``when input contains only the first parser SHOULD fail for no more input`` (c1: char) (c2:char) =
        failureShort (.>>.) c1 c2
        |> checkFailure "No more input"

    [<Property>]
    member x.``when fails on the first parser SHOULD specify the failure was the first parser`` (c1:char) (c2:char) (str:NonWhiteSpaceString) =
        ((str.Get.Length > 0) && (c1 <> c2))
        ==> lazy
        
        failure1stParse (.>>.) c1 c2 str.Get
        |> checkFailure  "Unexpected"

    [<Property>]
    member x.``when fails on the second parser SHOULD specify the failure was the second parser`` (c1: char) (c2:char) (str:NonWhiteSpaceString) =
        (str.Get.Length > 0 && c1 <> c2)
        ==> lazy
        
        failure2ndParse (.>>.) c1 c2 str.Get
        |> checkFailure  "Unexpected"

type ``-> discard second match`` () =
    [<Property>]
    member x.``when successful SHOULD return only the first match`` (c1: char) (c2: char) (str:NonWhiteSpaceString) =
        let result = successfulParse (.>>) c1 c2 str.Get
        result |> checkMatched c1

    [<Property>]
    member x.``when successful SHOULD remove both matches from the returned input`` (c1: char) (c2:char) (str:NonWhiteSpaceString) =
        let s = str.Get
        let result = successfulParse (.>>) c1 c2 s
        result |> checkRemaining s

    [<Property>]
    member x.``when input contains only the first parser SHOULD fail for no more input`` (c1: char) (c2:char) =
        failureShort (.>>) c1 c2
        |> checkFailure "No more input"

    [<Property>]
    member x.``when fails on the first parser SHOULD specify the failure was the first parser`` (c1:char) (c2:char) (str:NonWhiteSpaceString) =
        ((str.Get.Length > 0) && (c1 <> c2))
        ==> lazy
        
        failure1stParse (.>>) c1 c2 str.Get
        |> checkFailure  "Unexpected"

    [<Property>]
    member x.``when fails on the second parser SHOULD specify the failure was the second parser`` (c1: char) (c2:char) (str:NonWhiteSpaceString) =
        (str.Get.Length > 0 && c1 <> c2)
        ==> lazy
        
        failure2ndParse (.>>) c1 c2 str.Get
        |> checkFailure  "Unexpected"

type ``-> discard first match`` () =
    [<Property>]
    member x.``when successful SHOULD return only the first match`` (c1: char) (c2: char) (str:NonWhiteSpaceString) =
        let result = successfulParse (>>.) c1 c2 str.Get
        result |> checkMatched c2

    [<Property>]
    member x.``when successful SHOULD remove both matches from the returned input`` (c1: char) (c2:char) (str:NonWhiteSpaceString) =
        let s = str.Get
        let result = successfulParse (>>.) c1 c2 s
        result |> checkRemaining s

    [<Property>]
    member x.``when input contains only the first parser SHOULD fail for no more input`` (c1: char) (c2:char) =
        failureShort (>>.) c1 c2
        |> checkFailure "No more input"

    [<Property>]
    member x.``when fails on the first parser SHOULD specify the failure was the first parser`` (c1:char) (c2:char) (str:NonWhiteSpaceString) =
        ((str.Get.Length > 0) && (c1 <> c2))
        ==> lazy
        
        failure1stParse (>>.) c1 c2 str.Get
        |> checkFailure  "Unexpected"

    [<Property>]
    member x.``when fails on the second parser SHOULD specify the failure was the second parser`` (c1: char) (c2:char) (str:NonWhiteSpaceString) =
        (str.Get.Length > 0 && c1 <> c2)
        ==> lazy
        
        failure2ndParse (>>.) c1 c2 str.Get
        |> checkFailure  "Unexpected"