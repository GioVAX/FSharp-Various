module many

open FsCheck
open FsCheck.Xunit

open ParserTypes
open Parsers

open ParserTestsUtils

type ``many tests`` () =

    let _match f c s =
        let parser = c |> pchar |> many
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
    member x.``no matches SHOULD succeed AND match nothing`` (c:char) (s:NonEmptyString) =
        let s' = s.Get

        (s'.[0] <> c)
        ==> lazy
        let result = zeroMatches c s'

        result |> checkSuccess (fun s _ -> s |> List.isEmpty)

    [<Property>]
    member x.``no matches SHOULD succeed AND leave input unchanged`` (c:char) (s:NonEmptyString) =
        let s' = s.Get

        (s'.[0] <> c)
        ==> lazy
        let result = zeroMatches c s'

        result |> checkRemaining s'


