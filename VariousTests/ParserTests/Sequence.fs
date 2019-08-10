module sequence

open FsCheck
open FsCheck.Xunit

open ParserTypes
open Parsers

open ParserTestsUtils

let rand = new System.Random()

type ``sequence tests`` () =

    let inject char list =
        let idx = rand.Next(list |> List.length)
        let rec inner c i l = 
            match i, l with
            | 0, _ -> c :: l
            | _, h::t -> h::inner c (i-1) t
            | _, [] -> failwith "Index out of range"
        inner char idx list            

    let successTest (input:string) numElems =
        let expected = 
            input.Substring(0, numElems)
            |> List.ofSeq

        let parsers = 
            expected
            |> List.map pchar

        expected, run (Parsers.sequence parsers) input

    [<Property>]
    member x.``A successful match SHOULD match all the elements in the sequence`` (s:NonEmptyString) (num:PositiveInt) =
        let input = s.Get
        let numElems = num.Get

        (input.Length > 5 && numElems < input.Length)
        ==> lazy
            let expected, result  = successTest input numElems

            result |> checkMatched expected

    [<Property>]
    member x.``A successful match SHOULD remove all the matched from input`` (s:NonEmptyString) (num:PositiveInt) =
        let input = s.Get
        let numElems = num.Get

        (input.Length > 5 && numElems < input.Length)
        ==> lazy
            let _, result  = successTest input numElems

            let expected = input.Substring(numElems)
            result |> checkRemaining expected

    [<Property>]
    member x.``A sequence containing a non match SHOULD fail for the failing match`` (s:NonEmptyString) (c:char) (num:PositiveInt) =
        let input = s.Get
        let numElems = num.Get

        (input.Length > 5 && numElems < input.Length && not (input.Contains(c)) )
        ==> lazy
            let success = 
                input.Substring(0, numElems)
                |> List.ofSeq
            let expected = sprintf "Expecting '%c'." c

            let seq = inject c success
            let parsers = 
                seq
                |> List.map pchar

            let result = run (Parsers.sequence parsers) input

            result |> checkFailure expected