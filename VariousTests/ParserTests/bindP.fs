﻿module bindP

open FsCheck
open FsCheck.Xunit

open ParserTypes
open ParserMonad
open Parsers

open ParserTestsUtils

let private checkSecondIsCalled c1 c2 s parserBuilder =
    let f _ = pchar c2
    let p1 = pchar c1
    let input = buildInput c1 c2 s
    let parser = parserBuilder f p1

    let result = run parser input

    result
    |> checkRemaining s

let private checkSecondIsNOTCalled c1 c2 s parserBuilder =
    let f _ = failwith "This should not be called!"
    let p1 = pchar c2
    let input = buildInput c1 c2 s
    let parser = parserBuilder f p1

    let result = run parser input

    result
    |> checkFailure "Unexpected"

type ``-> prefix`` () =

    [<Property>]
    member x.``WHEN first parser succeeds, SHOULD call the second parser`` (c1: char) (c2: char) (s: NonEmptyString) =
        c1 <> c2 
        ==> lazy
        checkSecondIsCalled c1 c2 s.Get (fun f p -> bindP f p)

    [<Property>]
    member x.``WHEN first parser fails, SHOULD not call the second parser`` (c1: char) (c2: char) (s: NonEmptyString) =
        c1 <> c2 
        ==> lazy
        checkSecondIsNOTCalled c1 c2 s.Get (fun f p -> bindP f p)

type ``-> infix`` () =

    [<Property>]
    member x.``WHEN first parser succeeds, SHOULD call the second parser`` (c1: char) (c2: char) (s: NonEmptyString) =
        c1 <> c2 
        ==> lazy
        checkSecondIsCalled c1 c2 s.Get (fun f p -> p >>= f)

    [<Property>]
    member x.``WHEN first parser fails, SHOULD not call the second parser`` (c1: char) (c2: char) (s: NonEmptyString) =
        c1 <> c2 
        ==> lazy
        checkSecondIsNOTCalled c1 c2 s.Get (fun f p -> p >>= f)
