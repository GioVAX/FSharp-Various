module many1

open FsCheck
open FsCheck.Xunit

open ParserTypes
open Parsers

open ParserTestsUtils

type ``many1 tests`` () =

    let _match f c s =
        let parser = c |> pchar |> many1
        let input = f s
        
        run parser input

    let multipleMatches c s num = 
        let builder str = 
            let manyChars = 
                Array.init num (fun _ -> c )
                |> System.String
            manyChars + str
        
        _match builder c s

    let zeroMatches = _match id

    [<Property>]
    member x.``1 match SHOULD be matched`` (c:char) (s:NonEmptyString) =
        let s' = s.Get

        (s'.[0] <> c)
        ==> lazy
        let result = multipleMatches c s' 1

        result |> checkSuccess (fun s _ -> s.Length = 1)

    [<Property>]
    member x.``1 match SHOULD be removed from input`` (c:char) (s:NonEmptyString) =
        let s' = s.Get

        (s'.[0] <> c)
        ==> lazy
        let result = multipleMatches c s' 1

        result |> checkRemaining s'

    [<Property>]
    member x.``multiple matches SHOULD be all matched`` (c:char) (s:NonEmptyString) (num:PositiveInt) =
        let s' = s.Get

        (s'.[0] <> c)
        ==> lazy
        let num' = num.Get

        let result = multipleMatches c s' num'

        result |> checkSuccess (fun s _ -> s.Length = num')

    [<Property>]
    member x.``multiple matches SHOULD all be removed from input`` (c:char) (s:NonEmptyString) (num:PositiveInt) =
        let s' = s.Get

        (s'.[0] <> c)
        ==> lazy
        let result = multipleMatches c s' num.Get

        result |> checkRemaining s'

    [<Property>]
    member x.``no matches SHOULD fail`` (c:char) (s:NonEmptyString) =
        let s' = s.Get

        (s'.[0] <> c)
        ==> lazy
        let expected = c |> sprintf "Expecting '%c'."

        let result = zeroMatches c s'

        result |> checkFailure expected

