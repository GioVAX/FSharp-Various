module separatedBy

open FsCheck
open FsCheck.Xunit

open ParserTypes
open Parsers

open ParserTestsUtils

type ``separatedBy tests`` () =

    let removeLastSeparator (s:string) = s.[..s.Length-2]

    let successfulTest transform check nums sep (s:string)  =
        let nums' = nums |> List.ofArray
        let nums'' = nums' 
                    |> List.map (fun num -> sprintf "%i%c" num sep) 
                    |> List.fold (+) "" 
                    |> transform
        let input = nums'' + s
        let parser = separatedBy pint (sep |> pchar)

        let result = run parser input

        result |> check

    [<Property>]
    member x.``multiple matches with separators SHOULD match all`` (nums:NonEmptyArray<int>) (sep:char) (s:NonEmptyString) =
        let s' = s.Get
        (['0'..'9'] |> List.contains s'.[0] |> not 
            && ['0'..'9'] |> List.contains sep |> not)
        ==> lazy
        let nums' = nums.Get
        let test = successfulTest id (checkMatched (nums' |> List.ofArray))
        test nums' sep s.Get 

    [<Property>]
    member x.``multiple matches with separators SHOULD remove all matches and separators from input`` (nums:NonEmptyArray<int>) (sep:char) (s:NonEmptyString) =
        let s' = s.Get
        (['0'..'9'] |> List.contains s'.[0] |> not 
            && ['0'..'9'] |> List.contains sep |> not)
        ==> lazy
        let test = successfulTest id (checkRemaining s')
        test nums.Get sep s' 

    [<Property>]
    member x.``multiple matches with all separators except the last one SHOULD match all`` (nums:NonEmptyArray<int>) (sep:char) (s:NonEmptyString) =
        let s' = s.Get
        (['0'..'9'] |> List.contains s'.[0] |> not 
            && ['0'..'9'] |> List.contains sep |> not
            && s'.[0] <> sep)
        ==> lazy
        let nums' = nums.Get
        let test = successfulTest removeLastSeparator (checkMatched (nums' |> List.ofArray))
        test nums' sep s.Get 

    [<Property>]
    member x.``multiple matches with separators except the last oneSHOULD remove all matches and separators from input`` (nums:NonEmptyArray<int>) (sep:char) (s:NonEmptyString) =
        let s' = s.Get
        (['0'..'9'] |> List.contains s'.[0] |> not 
            && ['0'..'9'] |> List.contains sep |> not
            && s'.[0] <> sep)
        ==> lazy
        let test = successfulTest removeLastSeparator (checkRemaining s')
        test nums.Get sep s' 

    [<Property>]
    member x.``no matches with separators SHOULD succeed and match nothing`` (s:NonEmptyString) =
        let s' = s.Get
        (['0'..'9'] |> List.contains s'.[0] |> not )
        ==> lazy
        let sep = match s'.[0] with
                    | 'a' -> 'b'
                    | _ -> 'a'
        let parser = separatedBy pint (sep |> pchar)

        let result = run parser s'

        result |> checkMatched List.empty<int>

    [<Property>]
    member x.``no matches with separators SHOULD leave input unchanged`` (s:NonEmptyString) =
        let s' = s.Get
        (['0'..'9'] |> List.contains s'.[0] |> not )
        ==> lazy
        let sep = match s'.[0] with
                    | 'a' -> 'b'
                    | _ -> 'a'
        let parser = separatedBy pint (sep |> pchar)

        let result = run parser s'

        result |> checkRemaining s'
