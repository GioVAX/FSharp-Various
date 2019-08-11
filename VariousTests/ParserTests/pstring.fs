module pstring

open FsCheck
open FsCheck.Xunit
open FsUnit.Xunit

open ParserTypes
open Parsers

open ParserTestsUtils

type ``pstring tests`` () =

    [<Property>]
    member x.``a successful match SHOULD return the matched string`` (s1:NonEmptyString) (s2:NonEmptyString) =
        let s1' = s1.Get
        let input = s1' + s2.Get

        let result = run (pstring s1') input

        result |> checkMatched s1'

    [<Property>]
    member x.``a successful match SHOULD remove the matched string from the input`` (s1:NonEmptyString) (s2:NonEmptyString) =
        let s1' = s1.Get
        let s2' = s2.Get
        let input = s1' + s2'

        let result = run (pstring s1') input

        result |> checkRemaining s2'

    [<Property>]
    member x.``a failing match SHOULD fail for the wrong char`` (s1:NonEmptyString) (s2:NonEmptyString) (c:char) =
        let s1' = s1.Get

        not (s1'.Contains(c))
        ==> lazy
        let s1'' = s1' 
                    |> List.ofSeq 
                    |> injectRnd c 
                    |> Array.ofList
                    |> System.String
        let input = s1'' + s2.Get
        let expected = sprintf "Got '%c'" c

        let result = run (pstring s1') input

        match result with
        | Failure msg -> msg |> should endWith expected
        | Success _ -> failwith "This should have failed!!!!"

