module bindP

open FsCheck
open FsCheck.Xunit

open ParserTypes
open ParserMonad
open Parsers

open ParserTestsUtils

type ``-> bindP`` () =

    [<Property>]
    member x.``WHEN first parser succeeds, SHOULD call the second parser`` (c1: char) (c2: char) (s: NonEmptyString) =
        c1 <> c2 
        ==> lazy
        let f _ = pchar c2
        let p1 = pchar c1
        let input = buildInput c1 c2 s.Get

        let result = run (bindP f p1) input

        result
        |> checkRemaining s.Get

    [<Property>]
    member x.``WHEN first parser fails, SHOULD not call the second parser`` (c1: char) (c2: char) (s: NonEmptyString) =
        c1 <> c2 
        ==> lazy
        let f _ = failwith "This should not be called!"
        let p1 = pchar c2
        let input = buildInput c1 c2 s.Get

        let result = run (bindP f p1) input

        result
        |> checkFailure "Expecting"

type ``-> bindP infix`` () =
    [<Property>]
    member x.``WHEN first parser succeeds, SHOULD call the second parser`` (c1: char) (c2: char) (s: NonEmptyString) =
        c1 <> c2 
        ==> lazy
        let f _ = pchar c2
        let p1 = pchar c1
        let input = buildInput c1 c2 s.Get

        let result = run (p1 >>= f) input

        result
        |> checkRemaining s.Get

    [<Property>]
    member x.``WHEN first parser fails, SHOULD not call the second parser`` (c1: char) (c2: char) (s: NonEmptyString) =
        c1 <> c2 
        ==> lazy
        let f _ = failwith "This should not be called!"
        let p1 = pchar c2
        let input = buildInput c1 c2 s.Get

        let result = run (p1 >>= f) input

        result
        |> checkFailure "Expecting"